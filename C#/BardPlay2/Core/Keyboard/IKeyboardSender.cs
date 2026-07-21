namespace BardPlay2.Core.Keyboard;

/// <summary>
/// キー送信方式の共通インターフェース。
/// 仕様書 6章「Keyboard Sender」に対応。
/// SendInput / SendMessage / PostMessage を同じインターフェースで扱えるようにし、
/// 将来的な送信方式の追加(将来的に追加できる構造)を容易にする。
/// </summary>
public interface IKeyboardSender
{
    /// <summary>GUI表示用の名称 (例: "SendInput")</summary>
    string Name { get; }

    /// <summary>
    /// 演奏セッションの開始時に呼び出す。
    /// SendMessage/PostMessage系は、送信先ウインドウをここでリセットし、
    /// 次のKeyDownで改めてフォアグラウンドウインドウを取得し直す。
    /// </summary>
    void BeginSession();

    /// <summary>キーを押す</summary>
    void KeyDown(KeyStroke key);

    /// <summary>キーを離す</summary>
    void KeyUp(KeyStroke key);
}
