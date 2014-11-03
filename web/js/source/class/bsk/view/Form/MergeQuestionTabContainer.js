
/* ************************************************************************
  
#asset(qx/icon/Tango/16/actions/list-add.png)
#asset(qx/icon/Tango/16/actions/zoom-in.png)
#asset(qx/icon/Tango/16/actions/list-remove.png)

************************************************************************ */

qx.Class.define("bsk.view.Form.MergeQuestionTabContainer",
{
    //    extend : qx.ui.container.Composite,
    
    extend : bsk.view.Controller.AbstructTabController,

    construct: function(biz, tabModel, parentForm)
    {
        // this.controller = controller;
        this.biz = biz ;
        this.parentForm = parentForm;
        var mainlayout = new qx.ui.layout.HBox();
        mainlayout.setSpacing(2);
        
        this.base(arguments, mainlayout);
   
        this.tabModel = tabModel;

        this.table = new bsk.view.GenericTable(this, this.tabModel)
        this.table.setHeight(100);
        this.table.model.updateDataCellRenderers();
        
        this.add(this.table, {flex: 1});
        
        var butlayout = new qx.ui.layout.VBox();
        var butcnt = new qx.ui.container.Composite(butlayout);
        butlayout.setSpacing(2);
        
        this.mergeButton = new qx.ui.form.Button(null, "icon/16/actions/list-add.png");
        
        butcnt.add(this.mergeButton);
        
        this.add(butcnt);
        this.addListeners();
        
        console.log("<<")
    },

    members:
    {        
        attache_list: null,
        
        addButton : null,
        edtButton : null,
        remButton : null,

        setId: function(id){
            this.tabModel.vardata.id = id;
        },

        disableForm: function() {},
        enableForm: function() {},
        
        getExtraParams:  function() {
            return this.tabModel.vardata
        },
        
        submited : function(result) {
            this.table.model.clear();
            this.table.model.onRowDataIncome(result);
        },

        getActionUrl : function() {
            return this.tabModel.dblclick_action;
        },

        onCancelClick : function() {
            this.filterForm.form.reset();
        },
        
        refresh : function() {
            if(this.tabModel.vardata == undefined)
                return false;
            if(!this.filterForm){
                this.filterForm = new bsk.view.Form.GenericForm(this, undefined, this.tabModel.filter, undefined);
                this.filterForm.formFieldDescr = this.tabModel.vardata;
            }
            this.filterForm._onSubmitClick();
            console.log("&&")
            return true;
        },

        _onIncomeActionTabRowAction : function(response) {
            var result = response.getContent();
            if (false == bsk.util.errors.process(this, result))
                return false;
            this.refresh();
            return true;
        },
        
        rowDblClick : function(Row) {            
            this.biz.onAction(Row, this.filterForm.getValues(), this.tabModel.dblclick_action);
            this.refresh();
        },
                
        addListeners: function(){
            this.addListener("appear", this.refresh);
            /*  Слушатели это зло. Использовать их надо очень аккуратно.
             *  Но в данном случае, это необходимо.
             *  Например, если мы создали, дочерний элемент,
             *      то наш виджет никак не узнает о нем, пока не обновится.
             *  А обновится, он когда появится
             */
            
            this.mergeButton.addListener("execute", this._onMerge, this);

        },

        _onMerge: function(actionUrl){            
            var id = this.table.getSelectionModel().getSelectedRanges()[0];
            if (id == undefined)
                return false;
            var d = this.table.model.getData();
            var rowId = d[id.minIndex];
            var ROW = this.table.model.data[rowId];
            console.log("this.table.model = ", this.table.model);
            
            var req = new qx.io.remote.Request(this.tabModel.merge_action, "POST", "application/json");
            
            for(var key in ROW) {
                var val = ROW[key];
                req.setParameter(key, val, true);
            }

            var ExtraParams = this.getExtraParams()
            for(var key in ExtraParams) {
                if(key in ROW)
                    continue;
                var val = ExtraParams[key];
                req.setParameter(key, val, true);
            }
            
            req.addListener("completed", this._onIncomeActionTabRowAction, this);
            req.send();
            
            if(this.parentForm){
                this.parentForm._onCancelClick()
            }
            return true;
        }
        
    }
});
