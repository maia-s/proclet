#!/bin/bash -eu

run() {
    echo $*
    "$@"
}

for cargo in "cargo"; do
    for pm in "" "proc-macro,"; do
        for pm2 in "" "proc-macro2,"; do
            for lv in "" "literal-value,"; do
                features="$pm$pm2$lv"
                if [ ! -z "$features" ]; then
                    features="--features $features"
                fi
                RUSTFLAGS="-D warnings" run $cargo check -p proclet --no-default-features $features
            done
            features="$pm$pm2"
            if [ ! -z "$features" ]; then
                features="--features $features"
                run $cargo test -p proclet-tests $features
                if [ "$features" == "--features proc-macro,proc-macro2," ]; then
                    run $cargo test -p proclet-tests ${features}prefer-pm1
                fi
            fi
        done
    done
done
