Yukikaze
========

画像を管理するツールです。
未だ開発途上であるため一部の機能は動作せず、また特定の機能はURLを叩かないと起動できません。

テストとして利用するには、conf/application.conf中の『images.root=…』の部分に画像フォルダへのパスを入力してください。
別途 http://www.playframework.com/ よりPlay Framework(Scala)をダウンロードし、Yukikazeフォルダからplayを呼び出してください。
playコンソールからrunとタイプすると、アプリケーションが起動します。
 http://localhost:9000/ にアクセスできるはずです。
