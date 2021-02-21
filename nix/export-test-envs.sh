#!/bin/sh

#
# app
#

export EDU_MCI_LOG_ENV="dev"
export EDU_MCI_LOG_FORMAT="Bracket" # Bracket | JSON
export EDU_MCI_LOG_VERBOSITY="V3"
export EDU_MCI_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/edu-mci"
export EDU_MCI_ENDPOINT_PORT="3000"
