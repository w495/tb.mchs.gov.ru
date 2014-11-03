
/* ************************************************************************

************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("bsk.view.Controller.TabController",
{
    extend : bsk.view.Controller.AbstructTabController,

    construct : function(biz, Data, FilterVal, tabModel, use_toolbar)
    {
        this.base(arguments, new qx.ui.layout.VBox());

        this.tabModel = tabModel;
        this.initData = Data;
        this.biz = biz;
        
        this.gBox = new qx.ui.groupbox.GroupBox();
        this.gBox.setLayout(new qx.ui.layout.VBox(5));

        if(this.tabModel.legend != undefined)
            this.gBox.setLegend(this.tabModel.legend);

        if(undefined == use_toolbar || true == use_toolbar){
                this.toolbar = new bsk.view.ToolBar(this.biz,
                    this, this.tabModel.toolbar);
                this.add(this.toolbar);
        }
                
        this.add(this.gBox, {flex : 1});
        this.buildTable(this.tabModel, FilterVal);
        
        console.log("FilterVal  = ", FilterVal);
        
    },

    members : {

        /**
            Производит единовременное действие над строкой.
        **/
        _tabRowAction: function(actionUrl){
            var id = this.tab.getSelectionModel().getSelectedRanges()[0];
            if (id == undefined)
                return false;
            var d = this.tab.model.getData();
            var rowId = d[id.minIndex];
            var ROW = this.tab.model.data[rowId];
            var req = new qx.io.remote.Request(actionUrl, "POST", "application/json");
            for(var key in ROW) {
                var val = ROW[key];
                req.setParameter(key, val, true);
            }
            req.addListener("completed", this._onIncomeActionTabRowAction, this);
            req.send();
            return true;
        },

        /**
            Производит сложное действие над строкой.
        **/
        _tabRow:  function(actionUrl){
            var id = this.tab.getSelectionModel().getSelectedRanges()[0];
            if (id == undefined)
                return false;
            var d = this.tab.model.getData();
            var rowId = d[id.minIndex];
            // console.log("this.tab.model.data[rowId] = ", this.tab.model.data[rowId]);
            this.biz.onAction(this.tab.model.data[rowId], this.filterForm.getValues(), actionUrl);
            this.refresh();
            return true;
        },

        /**
            
        **/
        _tabChangeState: function(actionUrl){
            var id = this.tab.getSelectionModel().getSelectedRanges()[0];
            if (id == undefined)
                return false;
            var d = this.tab.model.getData();
            var rowId = d[id.minIndex];
            this.biz.onAction(this.tab.model.data[rowId], this.filterForm.getValues(), actionUrl);
            return true;
        },

        /**
            
        **/
        _tabChangeStateAction: function(actionUrl){
            // pass
        },

        /**
            Действие для любой кнопки,
            для которой не назначено определенное действие.
        **/
        _tabDefault: function(actionUrl){
            var vardata = {}
            if(this.tabModel.vardata)
                vardata = this.tabModel.vardata;
            vardata.isNew = true;
            this.biz.onAction(vardata, this.filterForm.getValues(), actionUrl);
            this.refresh();
        },

        _onIncomeActionTabRowAction : function(response) {
            var result = response.getContent();
            if (false == bsk.util.errors.process(this, result))
                return false;
            this.refresh();
            return true;
        },

        getActionUrl : function() {
            return this.tabModel.dblclick_action;
        },

        getExtraParams : function(params) {
            if(this.pager != undefined) {
                params.pager_offset = this.pager.getOffset();
                params.pager_limit = this.pager.getLimit();
                params.sort_column = this.tab.model.getSortColumnName();
                params.sort_direction = this.tab.model.getSortDirection();
            }
            if(this.tabModel.index_name != undefined
               && this.tabModel.index_name.length != undefined
               && this.tabModel.index_name.length > 0
               && this.initData != undefined ) {
                for(var k=0;k<this.tabModel.index_name.length;k++){
                    if ( this.initData[this.tabModel.index_name[k]] != undefined ){
                        params[this.tabModel.index_name[k]] = this.initData[this.tabModel.index_name[k]];
                    }
                }
            }
            return params;
        },

        buildTable : function(tabDescription, FilterVal) {
            this.biz.show_global_pb();
            
            // console.log("tabDescription = ", tabDescription);
            // console.log("FilterVal = ", FilterVal);
            
            if(tabDescription.filter == undefined) {
                alert("не определено обязательное поле filter в описании таблицы");
                return;
            }

            this.filterForm = new bsk.view.Form.GenericForm(this, undefined, tabDescription.filter, FilterVal);
            // this.tabModel.vardata
            //      --- измеяемая часть конфигурационных параметров.
            // В обычном случае она не опеределена.

            console.log("this.filterForm.formFieldDescr", this.filterForm.formFieldDescr)
            this.filterForm.formFieldDescr = this.tabModel.vardata;
            console.log("this.filterForm.formFieldDescr", this.filterForm.formFieldDescr)
            this.tab = new bsk.view.GenericTable(this, tabDescription);
            
            // console.log("tabDescription = ", tabDescription);

            if(tabDescription.selectionMode != undefined) {
                var sm = this.tab.getSelectionModel();
                sm.setSelectionMode(tabDescription.selectionMode);
            }

            if(tabDescription.tabHeight != undefined)
                this.tab.setHeight(tabDescription.tabHeight);

            this.tabDescription = tabDescription;

            this.gBox.add(this.tab, {flex:1});

            this.tab.model.updateDataCellRenderers();
            if(tabDescription.pager != undefined) {
                var pCnt = new qx.ui.container.Composite(new qx.ui.layout.Dock());
                this.gBox.add(pCnt);
                this.pager = new bsk.view.Pager(this, (tabDescription.pager.pageSize ) || 25);
                pCnt.add(this.pager, {edge:"east"});
            }
            this.filterForm._onSubmitClick(); // загружаем подефолту данные
            console.log('this.tab', this.tab);
        },

        submited : function(result) {
            if (bsk.util.errors.process(this, result)==false)
                return false;
            this.tab.model.clear();
            this.tab.model.onRowDataIncome(result);
            if(this.pager != undefined)
                this.pager.updatePager(result);
            this.biz.hide_global_pb();
            return true;
        },

        show_error : function(etype, emsg) {
            alert("Ошибка (" + etype + ") загрузки данных таблицы: " + emsg);
        },

        onSortChange : function() {
            if(this.pager != undefined) {
                this.filterForm._onSubmitClick(); // делаем запрос на сервер, только если есть пейджер
            }
        },

        placeForm : function(f) {
            this.gBox.add(f);
        },

        onPageChange : function() {
            this.filterForm._onSubmitClick(); 
        },

        bindVardata: function(data) {
            // this.tabModel.vardata
            //      --- измеяемая часть конфигурационных параметров.
            // В обычном случае она не опеределена.
            //
            // Обращаем внимение, что присваивание происходит по ссылке.
            // Это не клонирование.
            this.tabModel.vardata = data;
            this.filterForm.formFieldDescr = data;
        },

        refresh : function() {
            this.filterForm._onSubmitClick();
        },

        onCancelClick : function() {
            this.filterForm.form.reset();
        },

        rowDblClick : function(Row) {
            this.biz.onAction(Row, this.filterForm.getValues(), this.tabModel.dblclick_action);
            this.refresh();
        }
    }

});

