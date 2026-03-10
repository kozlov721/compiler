FROM haskell:9.2 AS builder

WORKDIR /compiler

# Copy dependency manifests first for better layer caching
COPY stack.yaml stack.yaml.lock package.yaml ./

RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the source and build the executable
COPY . .
RUN stack build --copy-bins --local-bin-path /out

# ── Runtime stage ────────────────────────────────────────────────────────────
FROM debian:bullseye-slim

# GCC is required at runtime to assemble and link the generated .s files
RUN apt-get update \
    && apt-get install -y --no-install-recommends gcc \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /out/compiler-exe /usr/local/bin/compiler

WORKDIR /src

ENTRYPOINT ["compiler"]
