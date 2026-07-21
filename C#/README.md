# BardPlay 2.0 (C# / .NET 10 / WinForms / NAudio)

Delphi版 [BardPlay](https://github.com/TakeHider/BardPlay) を元にC#で作り直したものです。

> **注意:** 
> Visual Studio 2022以降(.NET 10 SDK)で開き、NuGetから `NAudio` を復元してからビルドしてください。

## フォルダ構成

```
BardPlay2.sln
BardPlay2/
  BardPlay2.csproj
  Program.cs
  Core/
    Midi/
      MidiInputService.cs      MIDI入力(NAudio.Midi.MidiInのラッパー)
      MidiNoteEventArgs.cs
    Keyboard/
      IKeyboardSender.cs       送信方式の共通インターフェース
      KeyStroke.cs             1回のキー操作を表す値(VirtualKey/Shift/Ctrl/Alt)
      VirtualKeyTable.cs       キー名 → 仮想キーコード対応表
      NativeMethods.cs         SendInput/SendMessage/PostMessage用のWin32 P/Invoke
      SendInputKeyboardSender.cs
      SendMessageKeyboardSender.cs
      PostMessageKeyboardSender.cs
      KeyboardSenderFactory.cs
    Engine/
      IGameEngine.cs
      GameEngineBase.cs        共通処理(トランスポーズ、キー割り当て解決)
      FF14Engine.cs             モノフォニック
      BlueProtocolEngine.cs     ポリフォニック
      GameEngineFactory.cs
    PressedKeyManager.cs        押下中キーの管理(Add/Remove/Contains/ReleaseAll)
    Config/
      AppConfig.cs              アプリ設定(JSON保存)
      GameProfile.cs            ゲームプロファイルのモデル
      ConfigManager.cs          設定・プロファイルの読み書き
    Logging/
      DebugLogger.cs            デバッグ表示/ログ保存
      LogEntry.cs
  Profiles/
    FF14.json                   FF14用キーマッピング(既定値)
    BlueProtocol.json           Blue Protocol用キーマッピング(既定値)
  UI/
    MainForm.cs / MainForm.Designer.cs
```

仕様書13章のクラス構成にほぼ対応させています(UIのファイル分割のみWinForms流に
`.Designer.cs`へ分けています)。

## 設計方針の実装状況

1. **ゲーム固有仕様は `IGameEngine` に集約** → `FF14Engine` / `BlueProtocolEngine`
2. **送信方式は `IKeyboardSender` に集約** → `SendInputKeyboardSender` /
   `SendMessageKeyboardSender` / `PostMessageKeyboardSender`
3. **疎結合構成** → `MainForm` が MIDI入力・Config・Engine・Senderを組み立てる
   コンポジションルートになっており、各クラスは単体でテスト可能です。
4. **ゲーム追加時は新しい `GameEngine` を実装するだけ** →
   `GameEngineFactory` に1行足し、`Profiles/*.json` を1つ追加すれば拡張できます。

## WM_KEYUP の lParam

仕様書6章の要求どおり、`NativeMethods.BuildLParam()` で

- bit 0-15: リピートカウント(1)
- bit 16-23: `MapVirtualKey` で得たスキャンコード
- bit 30: 直前のキー状態(KeyUp時のみ1)
- bit 31: 遷移状態(KeyUp時のみ1)

を組み立てています。仕様書12章のデバッグ表示例
(`VK=A, ScanCode=1E, lParam=001E0001` / `lParam=C01E0001`)と一致する値になるように
実装・検算済みです。Blue Protocolでこの値が正しくないとNOTE_OFFが認識されない、という
仕様書の注記を踏まえ、SendMessage/PostMessageの両方式で同じロジックを共有しています。

## キーマッピングの編集

`Profiles/FF14.json` と `Profiles/BlueProtocol.json` は同梱の**サンプル**です
(FFXIVの数字キー行/QWERTY行/ASDF行を1オクターブずつ割り当てる、よく使われる
3オクターブ・クロマチック配列を仮に設定しています)。実際の演奏には、
ご自身のゲーム内キーバインド設定に合わせて `keyMapping` の中身
(MIDIノート番号 → キー名/Shift/Ctrl/Alt)を編集してください。
新しいゲームを追加する場合も、このJSON構造に沿ったファイルを`Profiles`フォルダに
追加し、対応する`IGameEngine`実装(モノ/ポリフォニックいずれか、または新規)を
`GameEngineFactory`に登録するだけで拡張できます。

キー名の一覧は `Core/Keyboard/VirtualKeyTable.cs` を参照してください。

## 未実装・今後の拡張ポイント(仕様書14章に対応)

- MIDIファイル自動演奏
- キーマッピング編集画面(現状はJSON直接編集)
- 疑似和音(VirtualChords)の同時押し順序制御 —
  設定項目としては保持していますが、NAudioがノートを1件ずつイベント配信するため、
  Delphi版のようなバッファ単位の昇順ソートは未実装です。必要であれば
  `MidiInputService` 側で短時間分のノートをバッファしてから
  昇順ソート・ディスパッチする拡張を追加してください。
- MIDIフィルタ(チャンネル・ベロシティ等)
- マクロ・自動演奏機能

## ビルド方法

1. Visual Studio 2022以降(.NET 10 SDKを含む)で `BardPlay2.sln` を開く
2. NuGetパッケージの復元(`NAudio`)
3. スタートアッププロジェクトを `BardPlay2` にしてビルド・実行
