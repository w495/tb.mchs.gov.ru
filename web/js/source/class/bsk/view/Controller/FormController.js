
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.Controller.FormController",
{
    //extend : qx.ui.groupbox.GroupBox,
    extend : qx.ui.container.Composite,

    construct : function(biz, Row, formModel, objName) {
        this.biz = biz;
        this.base(arguments, new qx.ui.layout.VBox());

        this.toolbar = new bsk.view.ToolBar(this.biz, this, formModel.toolbar);
        this.add(this.toolbar);

        this.formBox = new qx.ui.container.Composite();


        var label;
        if(Row == undefined || Row.isNew)
            label = "Создание " + objName;
        else
            label = "Редактирование " + objName;
        this.formCont = new qx.ui.groupbox.GroupBox(label);
        //this.formCont = new qx.ui.container.Composite();
        this.formCont.setLayout(new qx.ui.layout.VBox());

        if(formModel.centerForm == undefined || formModel.centerForm==true) {
            var d = new qx.ui.layout.Dock();
            d.setSort("y");
            this.formBox.setLayout(d);
            this.set({allowGrowX : true});

            var w1 = new qx.ui.core.Widget();
            var w2 = new qx.ui.core.Widget();
            var w3 = new qx.ui.core.Widget();
            var w4 = new qx.ui.core.Widget();
            this.formBox.add(w2, {edge:"west", flex:1})
            this.formBox.add(w3, {edge:"south", flex:1});
            this.formBox.add(w4, {edge:"east", flex:1});
            this.formBox.add(this.formCont, {edge:"center", flex:0});
        }
        else {
            this.formBox.setLayout(new qx.ui.layout.VBox());
            this.formBox.add(this.formCont, {flex:1});
        }

        this.add(this.formBox, {flex:1});
        if(formModel.controller == undefined) {
            this.form = new bsk.view.Form.GenericForm(this, Row, formModel);
        }
        else {
            this.form = bsk.view.StaticForms[formModel.controller](this, Row, formModel);
        }
    },

    members : {

        placeForm :function(f) {
           this.formCont.add(f, {flex:1});
        },

        submited : function(result) {
            this.biz.back();
        },

        onCancelClick : function() {
            this.biz.back();
        },

        getExtraParams : function(params) {
            return params;
        },

        disableForm : function() {
            this.formCont.setEnabled(false);
            this.biz.show_global_pb();
        },

        enableForm : function() {
            this.formCont.setEnabled(true);
            this.biz.hide_global_pb();
        },

        refresh : function() {
            if(this.form["refresh"])
                this.form.refresh();
        }

    }
});

