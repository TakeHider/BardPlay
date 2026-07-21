using NAudio.Midi;

namespace BardPlay2.Core.Midi;

/// <summary>
/// NAudioを使ったMIDI入力デバイスのラッパー。
/// 仕様書 3章「MIDI入力」に対応:
///  - MIDI入力デバイス一覧取得
///  - MIDIデバイス選択/接続/切断
///  - NOTE_ON / NOTE_OFF受信
///
/// NAudioのMidiInはバックグラウンドスレッド(メッセージポンプ用の隠しウインドウ)から
/// イベントを発火するため、イベント購読側でUIスレッドへのマーシャリングが必要な点に注意。
/// </summary>
public sealed class MidiInputService : IDisposable
{
    private MidiIn? _midiIn;

    public event EventHandler<MidiNoteEventArgs>? NoteOn;
    public event EventHandler<MidiNoteEventArgs>? NoteOff;
    public event EventHandler<string>? DeviceError;

    /// <summary>
    /// デバッグ用: 受信した生MIDIメッセージの内容を通知する(NoteOn/NoteOffとして
    /// 認識できなかったメッセージも含む)。NOTE_OFFが来ているのに反応しない、といった
    /// 調査に使う。
    /// </summary>
    public event EventHandler<string>? RawMessageReceived;

    public bool IsOpen => _midiIn != null;

    /// <summary>接続中のMIDI入力デバイス一覧を取得する。</summary>
    public static IReadOnlyList<string> GetDeviceNames()
    {
        var list = new List<string>();
        for (int i = 0; i < MidiIn.NumberOfDevices; i++)
        {
            list.Add(MidiIn.DeviceInfo(i).ProductName);
        }
        return list;
    }

    /// <summary>指定したデバイス番号のMIDI入力を開始する。</summary>
    public void Open(int deviceIndex)
    {
        Close();

        if (deviceIndex < 0 || deviceIndex >= MidiIn.NumberOfDevices)
        {
            DeviceError?.Invoke(this, "*** MIDI Device Error ***");
            return;
        }

        try
        {
            _midiIn = new MidiIn(deviceIndex);
            _midiIn.MessageReceived += OnMessageReceived;
            _midiIn.ErrorReceived += OnErrorReceived;
            _midiIn.Start();
        }
        catch (Exception)
        {
            _midiIn = null;
            DeviceError?.Invoke(this, "*** MIDI Device Error ***");
        }
    }

    /// <summary>MIDI入力を停止して閉じる。</summary>
    public void Close()
    {
        if (_midiIn == null) return;

        try
        {
            _midiIn.Stop();
        }
        catch
        {
            // 切断済み・デバイス切り離し済みなど、閉じる際のエラーは無視する
        }
        finally
        {
            _midiIn.MessageReceived -= OnMessageReceived;
            _midiIn.ErrorReceived -= OnErrorReceived;
            _midiIn.Dispose();
            _midiIn = null;
        }
    }

    private void OnMessageReceived(object? sender, MidiInMessageEventArgs e)
    {
        try
        {
            var midiEvent = e.MidiEvent;

            switch (midiEvent)
            {
                // NoteOnEvent/NoteEvent いずれの型で来ても、ステータスとベロシティで判定する。
                // Delphi版と同じロジック:
                //   ucStatus = NOTE_OFF ($80)
                //   もしくは ucStatus = NOTE_ON ($90) かつ ucData2(velocity) = $00
                // NAudioはvelocity=0のNOTE_ONを『型はNoteEvent、CommandCodeはNoteOnのまま』で
                // 返してくることがあるため、型ではなく CommandCode + Velocity で判定する。
                case NoteEvent noteEvt:
                    if (noteEvt.CommandCode == MidiCommandCode.NoteOff ||
                        (noteEvt.CommandCode == MidiCommandCode.NoteOn && noteEvt.Velocity == 0))
                    {
                        NoteOff?.Invoke(this, new MidiNoteEventArgs(noteEvt.NoteNumber, 0));
                    }
                    else if (noteEvt.CommandCode == MidiCommandCode.NoteOn && noteEvt.Velocity > 0)
                    {
                        NoteOn?.Invoke(this, new MidiNoteEventArgs(noteEvt.NoteNumber, noteEvt.Velocity));
                    }
                    break;

                default:
                    // Note On/Off以外(タイミングクロックやアクティブセンシング等のリアルタイム
                    // メッセージ、コントロールチェンジ等)は演奏に関係ないため無視する。
                    // 想定外のメッセージを追いたい時だけ、ここでRawMessageReceivedを発火させる。
                    if (midiEvent != null && !IsIgnorableRealtimeMessage(midiEvent.CommandCode))
                    {
                        RawMessageReceived?.Invoke(this,
                            $"未処理メッセージ: raw=0x{e.RawMessage:X6} type={midiEvent.GetType().Name} command={midiEvent.CommandCode}");
                    }
                    break;
            }
        }
        catch (Exception ex)
        {
            // ここで例外を握りつぶさずに外へ通知する。
            // MIDIコールバックのスレッドで例外を放置すると、以後イベントが一切
            // 発火しなくなる(NOTE_OFFだけ来なくなる、等)ことがあるため、
            // 必ずtry-catchで捕まえて可視化する。
            DeviceError?.Invoke(this, $"MIDIメッセージ処理中に例外が発生しました: {ex.Message}");
        }
    }

    /// <summary>
    /// タイミングクロックやアクティブセンシングなど、演奏には無関係で高頻度に送られてくる
    /// リアルタイムメッセージ(0xF8-0xFF)をログから除外するための判定。
    /// </summary>
    private static bool IsIgnorableRealtimeMessage(MidiCommandCode commandCode)
    {
        var code = (int)commandCode;
        return code >= 0xF8; // TimingClock, Start, Continue, Stop, AutoSensing, Reset 等
    }

    private void OnErrorReceived(object? sender, MidiInMessageEventArgs e)
    {
        DeviceError?.Invoke(this, "MIDI message error (invalid data received)");
    }

    public void Dispose() => Close();
}
