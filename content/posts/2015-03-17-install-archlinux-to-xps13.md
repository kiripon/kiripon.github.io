---
title: Arch Linux を Dell XPS 13(2015)へインストールする
date: 2015-03-17
tags: ["Arch Linux"]
---

#経緯
[Dell XPS 13](http://www.dell.com/jp/p/xps-13-9343-laptop/pd)を購入した.(とてもうれしい)
早速Arch Linux をインストールすることにしたが、配布されている ISO イメージでは，無線LANが認識されなかった．
そのためドライバがインストール済みのISOイメージを作成することにした．

#必要なドライバの確認
`lspci`コマンドで無線モジュールを確認する。

```
# lspci
...
02:00.0 Network controller: Broadcom Corporation BCM4352 802.11ac Wireless Network Adapter (rev 03)
```

調べたところ、`BCM4352`は
[broadcom-wl-dkms(AUR)](https://aur.archlinux.org/packages/broadcom-wl-dkms/)
を利用することで利用できるらしい。こいつを予め導入したインストールイメージをつくる。

#ISOイメージの作成
[Archiso(Arch Wiki)](https://wiki.archlinux.org/index.php/Archiso)に則って インストールイメージを作成する。
その際に、以下の変更を行った。

`yaourt`を使うため、`pacman.conf`の末尾に以下を追加

```
[archlinuxfr]
SigLevel = Never
Server = http://repo.archlinux.fr/$arch
```

`packages.both`の末尾に以下を追加
```
linux-headers
dkms
fakeroot
yaourt
```

`airootfs/root/customize_airootfs.sh`に以下を追加
```
# root で yaourt が実行できないのでユーザー権限で実行
sudo -u arch yaourt --noconfirm -S broadcom-wl-dkms
```

#Install
UEFI環境でSecure Bootを有効にしたまま起動するとHash Toolが起動する。
その時はArch Wikiの
[Unified Extensible Firmware Interface](https://wiki.archlinux.org/index.php/Unified_Extensible_Firmware_Interface#Secure_Boot)
の通りの手順を踏むと起動できる。

これで無線LANが有効になった状態で起動できるので、`wifi-menu`等で無線LANに繋いで通常のインストール手順をふめばよい。

#問題点
~~スピーカーから音が出ない。
[ここ](https://major.io/2015/02/03/linux-support-dell-xps-13-9343-2015-model/)とかを見る限り、いまのところ根本的な解決はないらしい。続報に期待~~


~~[https://github.com/mpalourdio/xps13](https://github.com/mpalourdio/xps13)
を参考にカーネルのブートオプションに`acpi_osi="Windows 2013"`を追加し、数回再起動したところスピーカーから音が出るようになった。~~

~~変更後のgummibootのエントリは以下のようになった。~~

```
title	Arch Linux
linux	/vmlinuz-linux
initrd	/initramfs-linux.img
options	enable_rc6=1 enable_fbc=1 lvds_downclock=1 pcie_aspm=force psmouse.resetafter=0 acpi_osi="!Windows 2013" root=/dev/sda7 rw
```

_5/24追記_
BIOS A02 以降からは`acpi_osi` オプションを指定しなくてもスピーカーが有効になるようになった。
