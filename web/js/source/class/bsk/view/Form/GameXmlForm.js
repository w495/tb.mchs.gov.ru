
qx.Class.define("bsk.view.Form.GameXmlForm",
{
    extend : bsk.view.Form.AbstractForm,

    construct : function(controller) {
        this.base(arguments, controller);
        this.buildForm();
    },

    members : {
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

            this.inpXml = new qx.ui.form.TextArea();

            this.inpXml.set({
                width : 900,
                height : 600
            });

            cnt.add(this.inpXml);

            var btnSubmit = new qx.ui.form.Button("Отправить");
            var btnCancel = new qx.ui.form.Button("Отмена");
            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            buttonRow.add(btnSubmit);
            buttonRow.add(btnCancel);

            cnt.add(buttonRow);

            btnSubmit.addListener("execute", this._onSubmitClick, this);
            btnCancel.addListener("execute", function(e){
                if(confirm("Отменить изменения?")) {
                    this.loadXml();
                }
            }, this);
 
            this.controller.placeForm(cnt);
            this.loadXml();

        },

        loadXml : function() {
            var req1 = new qx.io.remote.Request("/get-game-xml", "GET", "application/json");
            req1.addListener("completed", function(response) {
                var result = response.getContent();
                if (bsk.util.errors.process(this, result)==false) return false;
                this.inpXml.setValue(result.xml);
            }, this);
            req1.send();
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


       _dropInvalid : function() {
        },

        validateForm : function() {
            var flag = true;
            return flag;
        },

       _onSubmitClick : function(e) {
            if(this.validateForm()) {
                var req = new qx.io.remote.Request("/update-game-xml", "POST", "application/json");
                req.setParameter("xml", this.inpXml.getValue(), true);
                this.submit(req);
                this.openDialog("Данные сохранены", 200);
            }
        }
    }
});

