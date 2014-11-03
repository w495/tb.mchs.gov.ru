/* ************************************************************************

************************************************************************ */


qx.Class.define("bsk.view.Form.LogForm",
{

    extend : bsk.view.Form.BaseForm,

    construct : function(controller, Row) {
        if(Row)
            this.createNew = (Row.isNew == true);
        this.base(arguments, controller, Row);

        this.fStatus = "wdata";
        this.fData = null;
    },

    members : {
        
        inp : {
            Id                                      : null,
            Log_object_type_alias                   : null
        },

        drc : {  // download request config
            url: "/get-log-info",
            method: "GET",
            mimetype: "application/json"
        },


        buildForm : function(){
            
            this.inp.Id                     = new qx.ui.form.TextField();
            this.inp.Log_object_type_alias  = new qx.ui.form.TextField();

            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            var layout = new qx.ui.layout.Grid(2, 1);
            var cnt = new qx.ui.container.Composite(layout);
            var l1 = new qx.ui.basic.Label("Общая информация");
            
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var vertical_offset = 0;

            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});
 
            cnt.add(new qx.ui.basic.Label().set({value: "Id:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id, {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "Объект:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Log_object_type_alias, {row:vertical_offset , column:1});
            
            this.controller.placeForm(cnt);
            
            console.log("action ");
            this.cnt = cnt
            this.vertical_offset  = vertical_offset
            
            return {controller : cnt, offset: vertical_offset};
        }
        ,
        fillForm : function(data) {
            for(var fieldName in this.inp){
                var item = fieldName.toLowerCase();
                this.inp[fieldName].setValue(data.value[item])
            }
            
            var vertical_offset = this.vertical_offset + 1;
            var cnt = this.cnt;
            if(data.actions){
                for(var action in data.actions){
                    
                    var name = new qx.ui.form.TextField(data.actions[action].field_name);
                    var new_value = new qx.ui.form.TextField(data.actions[action].new_value);
                    var old_value = new qx.ui.form.TextField(data.actions[action].old_value);
                
                    if((data.actions[action].old_value == "") && (data.actions[action].new_value == "")){
                        continue;
                    }
                    
                    if(data.actions[action].field_name != ""){
                        cnt.add(new qx.ui.basic.Label().set({value: "Объект:",  rich : true}), {row:++vertical_offset, column:0});
                        cnt.add(name, {row:vertical_offset , column:1});
                    }
                    
                    if(data.actions[action].old_value != ""){
                        cnt.add(new qx.ui.basic.Label().set({value: "Старое значение:",  rich : true}), {row:++vertical_offset, column:0});
                        cnt.add(old_value, {row:vertical_offset , column:1});
                    }
                    
                    if(data.actions[action].new_value != ""){                    
                        cnt.add(new qx.ui.basic.Label().set({value: "Новое значение:",  rich : true}), {row:++vertical_offset, column:0});
                        cnt.add(new_value, {row:vertical_offset , column:1});
                    }
                    
                    console.log("action = ", data.actions[action])
                }
            }
            
        }
    }
});

