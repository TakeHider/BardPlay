namespace BardPlay2.UI;

partial class MainForm
{
    private System.ComponentModel.IContainer components = null;

    protected override void Dispose(bool disposing)
    {
        if (disposing && (components != null))
        {
            components.Dispose();
        }
        base.Dispose(disposing);
    }

    private Label lblMidiDevice;
    private ComboBox cboDevice;
    private Button btnRefresh;
    private Button btnStart;
    private GroupBox grpGame;
    private FlowLayoutPanel flowGame;
    private GroupBox grpSender;
    private FlowLayoutPanel flowSender;
    private Label lblOctave;
    private ComboBox cboOctave;
    private CheckBox chkVirtualChords;
    private CheckBox chkStartOnRun;
    private CheckBox chkDebug;
    private Label lblNowPlaying;
    private TextBox txtLog;
    private Label lblStatus;

    private void InitializeComponent()
    {
        this.components = new System.ComponentModel.Container();
        this.lblMidiDevice = new Label();
        this.cboDevice = new ComboBox();
        this.btnRefresh = new Button();
        this.btnStart = new Button();
        this.grpGame = new GroupBox();
        this.flowGame = new FlowLayoutPanel();
        this.grpSender = new GroupBox();
        this.flowSender = new FlowLayoutPanel();
        this.lblOctave = new Label();
        this.cboOctave = new ComboBox();
        this.chkVirtualChords = new CheckBox();
        this.chkStartOnRun = new CheckBox();
        this.chkDebug = new CheckBox();
        this.lblNowPlaying = new Label();
        this.txtLog = new TextBox();
        this.lblStatus = new Label();
        this.SuspendLayout();

        // lblMidiDevice
        this.lblMidiDevice.AutoSize = true;
        this.lblMidiDevice.Location = new Point(12, 15);
        this.lblMidiDevice.Text = "MIDI Device:";

        // cboDevice
        this.cboDevice.DropDownStyle = ComboBoxStyle.DropDownList;
        this.cboDevice.Location = new Point(100, 12);
        this.cboDevice.Size = new Size(260, 23);

        // btnRefresh
        this.btnRefresh.Location = new Point(368, 11);
        this.btnRefresh.Size = new Size(75, 25);
        this.btnRefresh.Text = "更新";
        this.btnRefresh.Click += new EventHandler(this.btnRefresh_Click);

        // btnStart
        this.btnStart.Location = new Point(449, 11);
        this.btnStart.Size = new Size(90, 25);
        this.btnStart.Text = "Start";
        this.btnStart.Click += new EventHandler(this.btnStart_Click);

        // grpGame
        this.grpGame.Location = new Point(12, 48);
        this.grpGame.Size = new Size(260, 70);
        this.grpGame.Text = "ゲーム";
        this.grpGame.Controls.Add(this.flowGame);

        // flowGame
        this.flowGame.Dock = DockStyle.Fill;
        this.flowGame.FlowDirection = FlowDirection.TopDown;
        this.flowGame.WrapContents = false;

        // grpSender
        this.grpSender.Location = new Point(280, 48);
        this.grpSender.Size = new Size(259, 90);
        this.grpSender.Text = "キーボード送信方式";
        this.grpSender.Controls.Add(this.flowSender);

        // flowSender
        this.flowSender.Dock = DockStyle.Fill;
        this.flowSender.FlowDirection = FlowDirection.TopDown;
        this.flowSender.WrapContents = false;

        // lblOctave
        this.lblOctave.AutoSize = true;
        this.lblOctave.Location = new Point(12, 128);
        this.lblOctave.Text = "オクターブ:";

        // cboOctave
        this.cboOctave.DropDownStyle = ComboBoxStyle.DropDownList;
        this.cboOctave.Location = new Point(100, 125);
        this.cboOctave.Size = new Size(80, 23);
        this.cboOctave.Items.AddRange(new object[] { "-2", "-1", "0", "+1", "+2" });
        this.cboOctave.SelectedIndexChanged += new EventHandler(this.cboOctave_SelectedIndexChanged);

        // chkVirtualChords
        this.chkVirtualChords.AutoSize = true;
        this.chkVirtualChords.Location = new Point(12, 155);
        this.chkVirtualChords.Text = "疑似和音(同時押しをソートして処理)";

        // chkStartOnRun
        this.chkStartOnRun.AutoSize = true;
        this.chkStartOnRun.Location = new Point(12, 180);
        this.chkStartOnRun.Text = "起動時に自動開始";

        // chkDebug
        this.chkDebug.AutoSize = true;
        this.chkDebug.Location = new Point(12, 205);
        this.chkDebug.Text = "デバッグ表示";
        this.chkDebug.CheckedChanged += new EventHandler(this.chkDebug_CheckedChanged);

        // lblNowPlaying
        this.lblNowPlaying.AutoSize = true;
        this.lblNowPlaying.Location = new Point(12, 232);
        this.lblNowPlaying.Text = "演奏情報 / デバッグログ:";

        // txtLog
        this.txtLog.Location = new Point(12, 252);
        this.txtLog.Size = new Size(527, 220);
        this.txtLog.Multiline = true;
        this.txtLog.ReadOnly = true;
        this.txtLog.ScrollBars = ScrollBars.Vertical;
        this.txtLog.Font = new Font("Consolas", 9F);

        // lblStatus
        this.lblStatus.AutoSize = true;
        this.lblStatus.Location = new Point(12, 480);
        this.lblStatus.Text = "停止中";

        // MainForm
        this.AutoScaleDimensions = new SizeF(7F, 15F);
        this.AutoScaleMode = AutoScaleMode.Font;
        this.ClientSize = new Size(551, 505);
        this.Controls.Add(this.lblMidiDevice);
        this.Controls.Add(this.cboDevice);
        this.Controls.Add(this.btnRefresh);
        this.Controls.Add(this.btnStart);
        this.Controls.Add(this.grpGame);
        this.Controls.Add(this.grpSender);
        this.Controls.Add(this.lblOctave);
        this.Controls.Add(this.cboOctave);
        this.Controls.Add(this.chkVirtualChords);
        this.Controls.Add(this.chkStartOnRun);
        this.Controls.Add(this.chkDebug);
        this.Controls.Add(this.lblNowPlaying);
        this.Controls.Add(this.txtLog);
        this.Controls.Add(this.lblStatus);
        this.FormBorderStyle = FormBorderStyle.FixedSingle;
        this.MaximizeBox = false;
        this.Text = "BardPlay 2.0";
        this.Load += new EventHandler(this.MainForm_Load);
        this.FormClosing += new FormClosingEventHandler(this.MainForm_FormClosing);
        this.ResumeLayout(false);
        this.PerformLayout();
    }
}
