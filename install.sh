#!/bin/sh
# nesmacs installer: puts the AOT jar and a launcher under $PREFIX (default
# ~/.local). No sudo, no Clojure CLI, no build tools.
#
#   curl -fsSL https://raw.githubusercontent.com/nez/nesmacs/main/install.sh | sh
#   ./install.sh --prefix ~/.local --version v1.0.0-rc1
#   ./install.sh --uninstall
#
# Run from a checkout it uses (or builds) target/nesmacs.jar instead of
# downloading a release. There is no checksum step: the jar comes over TLS from
# the release it was published to.
set -eu

REPO="${NESMACS_REPO:-nez/nesmacs}"
VERSION="${NESMACS_VERSION:-latest}"
PREFIX="${NESMACS_PREFIX:-$HOME/.local}"
UNINSTALL=0

usage() {
  cat <<'EOF'
Usage: install.sh [--prefix DIR] [--version TAG] [--uninstall]

  --prefix DIR   install under DIR (default: ~/.local)
  --version TAG  release to download (default: latest)
  --uninstall    remove the jar and launcher

Env: NESMACS_PREFIX, NESMACS_VERSION, NESMACS_REPO
Requires: Java 17+, tmux, git, ripgrep.
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --prefix) PREFIX="$2"; shift 2 ;;
    --version) VERSION="$2"; shift 2 ;;
    --uninstall) UNINSTALL=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

SHARE="$PREFIX/share/nesmacs"
BIN="$PREFIX/bin"
JAR="$SHARE/nesmacs.jar"
LAUNCHER="$BIN/nesmacs"

# The script's own directory, when it has one (piped to `sh`, $0 is not a path).
SELF_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" 2>/dev/null && pwd || echo "")
CHECKOUT=0
if [ -n "$SELF_DIR" ] && [ -f "$SELF_DIR/deps.edn" ] && [ -d "$SELF_DIR/src/nesmacs" ]; then
  CHECKOUT=1
fi

if [ "$UNINSTALL" = 1 ]; then
  rm -f "$LAUNCHER" "$JAR"
  rmdir "$SHARE" 2>/dev/null || true
  echo "Removed $LAUNCHER and $JAR (your ~/.nesmacs data is untouched)."
  exit 0
fi

# --- Prerequisites ---
fail=0
if command -v java >/dev/null 2>&1; then
  major=$(java -version 2>&1 | awk -F'"' '/version/ {print $2}' \
          | awk -F. '{ if ($1 == "1") print $2; else print $1 }')
  if [ -n "$major" ] && [ "$major" -ge 17 ] 2>/dev/null; then
    echo "[ok] Java $major"
  else
    echo "[!!] need Java 17+, found ${major:-unknown}" >&2
    fail=1
  fi
else
  echo "[!!] java not found — install JDK 17+ (apt install openjdk-21-jdk / brew install openjdk)" >&2
  fail=1
fi

for tool in tmux git rg; do
  if command -v "$tool" >/dev/null 2>&1; then
    echo "[ok] $tool"
  else
    echo "[!!] $tool not found — install it (apt install $tool / brew install $tool)" >&2
    fail=1
  fi
done
[ "$fail" = 0 ] || { echo "Install the missing tools and re-run." >&2; exit 1; }

# --- Get the jar ---
tmp=""
if [ "$CHECKOUT" = 1 ] && [ -f "$SELF_DIR/target/nesmacs.jar" ]; then
  jar_src="$SELF_DIR/target/nesmacs.jar"
  echo "[ok] using $jar_src"
elif [ "$CHECKOUT" = 1 ] && command -v clojure >/dev/null 2>&1; then
  echo "Building target/nesmacs.jar..."
  ( cd "$SELF_DIR" && make jar )
  jar_src="$SELF_DIR/target/nesmacs.jar"
else
  if [ "$VERSION" = latest ]; then
    url="https://github.com/$REPO/releases/latest/download/nesmacs.jar"
  else
    url="https://github.com/$REPO/releases/download/$VERSION/nesmacs.jar"
  fi
  tmp=$(mktemp)
  trap 'rm -f "$tmp"' EXIT HUP INT TERM
  echo "Downloading $url ..."
  if command -v curl >/dev/null 2>&1; then
    curl -fL --progress-bar "$url" -o "$tmp"
  elif command -v wget >/dev/null 2>&1; then
    wget -q --show-progress "$url" -O "$tmp"
  else
    echo "[!!] curl or wget is required to download the release" >&2
    exit 1
  fi
  jar_src="$tmp"
fi

mkdir -p "$SHARE" "$BIN"
cp "$jar_src" "$JAR"

cat > "$LAUNCHER" <<EOF
#!/bin/sh
exec java -jar "$JAR" "\$@"
EOF
chmod +x "$LAUNCHER"

# --- Migrate a pre-rebrand config dir ---
if [ -d "$HOME/.xmas" ] && [ ! -e "$HOME/.nesmacs" ]; then
  mv "$HOME/.xmas" "$HOME/.nesmacs"
  echo "[ok] moved ~/.xmas to ~/.nesmacs"
fi
mkdir -p "$HOME/.nesmacs"

echo ""
echo "=== Installed ==="
echo "  $LAUNCHER"
case ":$PATH:" in
  *":$BIN:"*) ;;
  *) echo "  Add it to PATH:  export PATH=\"$BIN:\$PATH\"" ;;
esac
echo "  Config and session live in ~/.nesmacs/"
