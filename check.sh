#!/bin/bash
bash submodules/ascii-check/run.sh src
bash submodules/ascii-check/run.sh --whitelist "🦊" test
bash submodules/ascii-check/run.sh spec
