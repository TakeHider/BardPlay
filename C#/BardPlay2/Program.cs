using BardPlay2.UI;

namespace BardPlay2;

internal static class Program
{
    /// <summary>
    ///  アプリケーションのメイン エントリ ポイントです。
    /// </summary>
    [STAThread]
    private static void Main()
    {
        ApplicationConfiguration.Initialize();

        // 未処理例外はデバッグログへ回収してから落ちる(原因調査しやすくするため)
        Application.ThreadException += (_, e) =>
            MessageBox.Show(e.Exception.ToString(), "BardPlay2 - 予期しないエラー",
                MessageBoxButtons.OK, MessageBoxIcon.Error);

        AppDomain.CurrentDomain.UnhandledException += (_, e) =>
            MessageBox.Show(e.ExceptionObject?.ToString(), "BardPlay2 - 致命的なエラー",
                MessageBoxButtons.OK, MessageBoxIcon.Error);

        Application.Run(new MainForm());
    }
}
