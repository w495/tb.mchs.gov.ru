
qx.Class.define("bsk.view.Form.GameEditorForm",
{
    extend : bsk.view.Form.AbstractForm,

    construct : function(controller) {
        this.base(arguments, controller);
        this.id = "";
        this.mapName = "";
        this.buildForm();
    },

    members : {
        disableWidgets : function(WList) {
            for(var i=0; i<WList.length; i++)
                WList[i].setEnabled(false);
        },

        showEMsg : function(fieldName, msg) {
            var fField = undefined;
            if(fField != undefined) {
                fField.setInvalidMessage(msg);
                fField.setValid(false);
            }
            else {
                alert("Ошибка сервера - " + msg + " для " + fieldName);
            }
        },

        buildForm : function() {
            var cnt = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            var toolBar = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            var flashBar = new qx.ui.container.Composite(new qx.ui.layout.HBox());

            cnt.add(toolBar);
            cnt.add(flashBar);
            flashBar.setWidth(1000);
            flashBar.setHeight(600);

            this.flashEditor = new qx.ui.embed.Flash("resource/bsk/flash/editor.swf").set({
                scale: "noscale",
                variables : {
                    xml:"resource/bsk/flash/visuals.xml",
                    db:"resource/bsk/flash/db.xml"
                }
            });

            var btnNew =  new qx.ui.form.Button("Создать");
            var btnOpen =  new qx.ui.form.Button("Открыть");
            var btnSave =  new qx.ui.form.Button("Сохранить");

            toolBar.add(btnNew);
            toolBar.add(btnOpen);
            toolBar.add(btnSave);

            flashBar.add(this.flashEditor, {flex: 1});
 
            this.controller.placeForm(cnt);

            btnOpen.addListener("execute", this._onBtnOpenClick, this);
            btnNew.addListener("execute", this._onBtnNewClick, this);
            btnSave.addListener("execute", this._onBtnSaveClick, this);
        },

        openDialog : function(info, width) {
            this.win = new qx.ui.window.Window(info);
            this.win.setWidth(width);
            this.win.setLayout(new qx.ui.layout.HBox(6));
            this.win.setModal(true);
            this.win.setAllowMaximize(false);
            this.win.setAllowMinimize(false);
            this.win.open();
            this.controller.biz.getRoot().add(this.win, {left:250, top:250});
            var btnOk = new qx.ui.form.Button("Закрыть");
            this.win.add(btnOk);
                                                                                                                                                                
            btnOk.addListener("execute", function() {
                this.win.close();
            }, this);

        },

        _onBtnOpenClick : function(e) {
            this.win = new qx.ui.window.Window("Открыть карту");
            var layout = new qx.ui.layout.Grid(12, 6);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            this.win.setLayout(layout);
            this.win.setModal(true);
            this.win.setAllowMaximize(false);
            this.win.setAllowMinimize(false);
            this.win.open();
            this.controller.biz.getRoot().add(this.win, {left:250, top:250});
            this.selMapName = new qx.ui.form.SelectBox();
            this.selMapName.itemMap = {};
            var req1 = new qx.io.remote.Request("/get-game-maps", "GET", "application/json");
            req1.addListener("completed", function(response) {
                var result = response.getContent();
                if (bsk.util.errors.process(this, result)==false) return false;
                for(var j=0; j<result.values.length; j++) {
                    var SI = result.values[j];
                    var selItem = new qx.ui.form.ListItem(SI.name, null, SI.id);
                    this.selMapName.itemMap[SI.id] = selItem;
                    this.selMapName.add(selItem); 
                }
            }, this);
            req1.send();

            this.win.add(new qx.ui.basic.Label("Название:"), {row:0, column:0});
            this.win.add(this.selMapName, {row:0, column:1});

            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);

            var btnSave = new qx.ui.form.Button("Открыть");
            var btnCancel = new qx.ui.form.Button("Отмена");
            buttonRow.add(btnSave);
            buttonRow.add(btnCancel);

            this.win.add(buttonRow, {row:1, column:0, colSpan:2});
                                                                                                                                                                
            btnSave.addListener("execute", function() {
                var items = this.selMapName.getSelectables();
                var id = items[0].getModel();
                
                var req = new qx.io.remote.Request("/load-game-map", "POST", "application/json");
                req.setParameter("id", id, true);
                req.send();
                req.addListener("completed", function(response) {
                    var result = response.getContent();
                    if (bsk.util.errors.process(this, result)==false) return false;
                    var Map = bsk.util.utils.decode(result.data);
                    var FE = this.flashEditor.getFlashElement();
                    FE.loadMap(Map);
                    this.id = result.id;
                    this.mapName = result.name;
                    this.win.close();
                }, this);
            }, this);

            btnCancel.addListener("execute", function() {
                this.win.close();
            }, this);
        },

        _onBtnSaveClick : function(e) {
            this.win = new qx.ui.window.Window("Сохранить карту как");
            var layout = new qx.ui.layout.Grid(12, 6);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            this.win.setLayout(layout);
            this.win.setModal(true);
            this.win.setAllowMaximize(false);
            this.win.setAllowMinimize(false);
            this.win.open();
            this.controller.biz.getRoot().add(this.win, {left:250, top:250});
            this.inpSaveAsName = new qx.ui.form.TextField();
            this.inpSaveAsName.setWidth(200);
            this.inpSaveAsName.setValue(this.mapName);
            this.win.add(new qx.ui.basic.Label("Название:"), {row:0, column:0});
            this.win.add(this.inpSaveAsName, {row:0, column:1});

            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);

            var btnSave = new qx.ui.form.Button("Сохранить");
            var btnCancel = new qx.ui.form.Button("Отмена");
            buttonRow.add(btnSave);
            buttonRow.add(btnCancel);

            this.win.add(buttonRow, {row:1, column:0, colSpan:2});
                                                                                                                                                                
            btnSave.addListener("execute", function() {
                this.newMapName = this.inpSaveAsName.getValue();
                this.win.close();
                this._onSubmitClick();
            }, this);

            btnCancel.addListener("execute", function() {
                this.win.close();
            }, this);
        },


         _onBtnNewClick : function(e) {
            this.win = new qx.ui.window.Window("Создать новую карту");
            var layout = new qx.ui.layout.Grid(12, 6);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            this.win.setLayout(layout);
            this.win.setWidth(200);

            this.win.setModal(true);
            this.win.setAllowMaximize(false);
            this.win.setAllowMinimize(false);
            this.win.open();

            this.spWidth = new qx.ui.form.Spinner(1, 50, 100);
            this.spHeight = new qx.ui.form.Spinner(1, 50, 100);
            

            this.win.add(new qx.ui.basic.Label("Ширина:"), {row:0, column:0});
            this.win.add(this.spWidth, {row:0, column:1});
            this.win.add(new qx.ui.basic.Label("Высота:"), {row:1, column:0});
            this.win.add(this.spHeight, {row:1, column:1});

            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);


            var btnSave = new qx.ui.form.Button("Создать");
            var btnCancel = new qx.ui.form.Button("Отмена");
            buttonRow.add(btnSave);
            buttonRow.add(btnCancel);

            this.win.add(buttonRow, {row:2, column:0, colSpan:2});
                                                                                                                                                                
            btnSave.addListener("execute", function() {
                this.flashEditor.getFlashElement().newMap(this.spWidth.getValue(), this.spHeight.getValue());
                this.win.close();
            }, this);

            btnCancel.addListener("execute", function() {
                this.win.close();
            }, this);


            this.controller.biz.getRoot().add(this.win, {left:250, top:250});
        },

       _dropInvalid : function() {
        },

        validateForm : function() {
            var flag = true;
            return flag;
        },

       _onSubmitClick : function(e) {
            this._dropInvalid();
            if(this.mapName != this.newMapName)
                this.id = "";

            if(this.validateForm()) {
                var req = new qx.io.remote.Request("/update-game-map", "POST", "application/json");
                var mapData = this.flashEditor.getFlashElement().saveMap();
                req.setParameter("id", this.id, true);
                req.setParameter("name", this.newMapName, true);
                var D = bsk.util.utils.encode(mapData);
                req.setParameter("data", D, true);
                this.submit(req);
                this.openDialog("Карта сохранена", 200);
            }
        }

    }
});

