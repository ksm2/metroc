pkgname="metroc"
pkgver=$(sed -En 's/^version:\s+(.*)/\1/p' metroc.cabal)
pkgrel=1
pkgdesc="Metro is a garbage collected, compiled language for the Web and Server."
arch=("x86_64")
url="https://metrolang.org/"
license=("MIT")
depends=("gmp")
source=("$pkgname"
        "LICENSE")
sha256sums=($(sha256sum metroc | cut -d ' ' -f 1)
            "f26af038da0c9fe24d26f950ff05d58ea71a281847d335158b4f51f95c4080e0")

package() {
  install -Dm0755 $pkgname "$pkgdir/usr/bin/$pkgname"
  install -Dm0644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
