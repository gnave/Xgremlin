#
# extracts the version number from Version.h
#
$2 ~ /VERSION/ { print substr($3,2,length($3)-2) }
