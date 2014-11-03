
/* ************************************************************************

    Описание абстрактного табличного контроллера.
    Зачем:
        * чтобы вынести некоторые функции не зависящие от членов класса;
        * описать, что должно быть в табличном контроллере;
        * может понадобиться если мы будем менять форму табличного контроллерa.

    Возможно стоит сюда вынести еще что-то

************************************************************************* */

qx.Class.define("bsk.view.Controller.AbstructTabController",
{
    type : "abstract",

    extend : qx.ui.container.Composite,

    construct : function(layout)
    {
        this.base(arguments, layout);
        
        ////// this.addListener("appear", this.refresh);
        /*  Слушатели это зло. Использовать их надо очень аккуратно.
         *  Но в данном случае, это необходимо.
         *  Например, если мы создали, дочерний элемент,
         *      то наш виджет никак не узнает о нем, пока не обновится.
         *  А обновится, он когда появится
         */
    },

    members : {

        tabModel    : undefined,
        initData    : undefined,
        biz         : undefined,
        vardata     : undefined,
        filterForm  : undefined,
        pager       : undefined,
        tab         : undefined,
        gBox        : undefined,

        disableForm : function() {
            this.biz.show_global_pb();
        },

        enableForm : function() {
            this.gBox.setEnabled(true);
            this.biz.hide_global_pb();
        },

        show_error : function(etype, emsg) {
            alert("Ошибка (" + etype + ") выполнения команды: " + emsg);
        },

        onToolbarBtn : function(actionUrl, specParam) {
            switch(specParam){
                case "tab-row-action":
                    // Нажали кнопки для единовременного действия над строкой.
                    // Например, удаления.
                    this._tabRowAction(actionUrl);
                    break;
                case "tab-row":
                    // Нажали кнопки для сложного действия над строкой.
                    // Например, редактирования.
                    this._tabRow(actionUrl);
                    break;
                case "tab-change-state":
                    // Нажали кнопки для изменения состояния модели.
                    this._tabChangeState(actionUrl);
                    break;
                case "tab-change-state-action":
                    // Нажали кнопки для изменения состояния модели.
                    this._tabChangeStateAction(actionUrl);
                    break;
                default:
                    // Нажали любую иную кнопку
                    this._tabDefault(actionUrl);
            }
        },


        /*
            Абстрактные методы:
        */


        _tabRowAction: function(actionUrl){},

        _tabRow:  function(actionUrl){},

        _tabChangeState: function(actionUrl){},

        _tabChangeStateAction: function(actionUrl){},

        _tabDefault: function(actionUrl){},

        getExtraParams : function(params) {
            return params;
        },

        _onIncomeActionTabRowAction : function(response) {},

        buildTable : function(tabDescription, FilterVal) {},

        submited : function(result) {},

        onSortChange : function() {},

        placeForm : function(f) {},

        onPageChange : function() {},

        bindVardata: function(data) {},

        refresh : function() {},

        onCancelClick : function() {},

        rowDblClick : function(Row) {}
    }

});

