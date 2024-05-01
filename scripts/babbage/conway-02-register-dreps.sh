#!/usr/bin/env bash

# Use this script after conway-01-setup-new-delegator-keys.sh
# Run this script to register Dreps and Delegate to Dreps.

# This scripts uses set -x to show in terminal the commands executed by the script. Remove or comment set -x to disable this behavior
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -x
set -euo pipefail

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

sprocket() {
  if [ "$UNAME" == "Windows_NT" ]; then
    # Named pipes names on Windows must have the structure: "\\.\pipe\PipeName"
    # See https://docs.microsoft.com/en-us/windows/win32/ipc/pipe-names
    echo -n '\\.\pipe\'
    echo "$1" | sed 's|/|\\|g'
  else
    echo "$1"
  fi
}

DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$DREP_DIR"

# GENERATE DREP KEYS

for i in {1..3}; do
  cardano-cli conway governance drep key-gen \
    --verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey"
done

# GENERATE AND SUBMIT REGISTRATION CERTIFICATES

drepDeposit=$(cardano-cli conway query gov-state | jq -r .currentPParams.dRepDeposit)

for i in {1..3}; do
  cardano-cli conway governance drep registration-certificate \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --key-reg-deposit-amt "$drepDeposit" \
    --out-file "${TRANSACTIONS_DIR}/drep${i}-reg.cert"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/drep1-reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep2-reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep3-reg.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/drep-reg-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-reg-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/drep-reg-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/drep-reg-tx.signed"

cardano-cli query tip

sleep 5

# GENERATE DELEGATION CERTIFICATES FROM STAKE ADDRESSES TO THE DREPS

for i in {1..3}; do
  cardano-cli conway stake-address vote-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/drep-deleg${i}.cert"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg1.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg2.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg3.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-deleg-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --signing-key-file "${UTXO_DIR}/stake2.skey" \
  --signing-key-file "${UTXO_DIR}/stake3.skey" \
  --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/drep-deleg-tx.signed"

cardano-cli query tip --testnet-magic 42

sleep 5

# QUERY DREP STATE TO CONFIRM

cardano-cli conway query drep-state --all-dreps
