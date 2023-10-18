#!/bin/sh

set -e

lein install
lein uberjar
SRC_JAR=`ls target/fractl-*standalone*.jar`
BASENAME=$(basename -- "$SRC_JAR")
FILENAME="${BASENAME%.*}"
NORM_FILENAME=${FILENAME%-standalone*}

TARGET_DIR=`realpath ${1:-$HOME}`/$NORM_FILENAME
TARGET_JAR=$TARGET_DIR/$BASENAME

mkdir -p $TARGET_DIR

cp target/fractl-*standalone*.jar $TARGET_JAR
cp config.edn $TARGET_DIR

cat << EOF > $TARGET_DIR/fractl
#!/bin/sh
java -jar ${TARGET_JAR} \$@
EOF

chmod +x $TARGET_DIR/fractl

echo "fractl installed to" $TARGET_DIR
