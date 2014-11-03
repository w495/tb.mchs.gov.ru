/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.GenericTableModel",
{
    //extend : qx.ui.table.model.Remote,
    extend : qx.ui.table.model.Simple,
 
    construct : function(cntl, tab, tabDescription) {
        this.base(arguments);
        this.cntl = cntl;
        this.tab = tab;
        this.data = {};
        this.tabDescription = tabDescription;
        this.createColumns(tabDescription);
        //this.asc = tabDescription.ascending || true;
        this.asc = tabDescription.ascending;
        this.addListener("sorted", this.onSortChange, this);
        //this.loadRowData(this.tabDescription.filter.url);
    },

    members :
    {
        findSortColumnName : function() {
            var ret = null;
            var i = 0;
            for(var k in this.columnsDesc) {
                if(i == this.sortIndex) {
                    ret = this.columnsDesc[k];
                    break;
                }
                i++;
            }
            return ret;
        },

        getSortColumnName : function() {
            return this.sortColumn.name;
        },

        getSortDirection : function() {
            if(this.isSortAscending())
                return "asc";
            return "desc";
        },

        onSortChange : function(e) {
            this.sortIndex = this.getSortColumnIndex();
            this.sortColumn = this.findSortColumnName();
            this.asc = this.isSortAscending();

            this.cntl.onSortChange();
        },

        createColumns : function(tabDescription) {
            this.sortIndex = 0;
            var colNames = [];
            var colAlias = [];
            this.columnsDesc = {};
            for(var i=0; i<tabDescription.columns.length; i++) {
                var I = tabDescription.columns[i];
                I.columnIndex = i;
                colNames.push(I.name);
                colAlias.push(I.alias);
                this.columnsDesc[I.name] = I;
                this.setColumnSortable(i, I.sortable);
                if(tabDescription.sort == I.name) {
                    this.sortColumn = I;
                    this.sortIndex = i;
                }
            }
            this.setColumns(colAlias, colNames);
        },

        mySort: function(row1, row2, sortby, ascending){
            var sec1 = Math.round(row1[sortby].substr(row1[sortby].lastIndexOf(":")+1));
            var min1 = Math.round( row1[sortby].substr(row1[sortby].indexOf(":")+1,2) );
            var hrs1 = Math.round( row1[sortby].substr(0,row1[sortby].indexOf(":")) );
            var sec2 = Math.round(row2[sortby].substr(row2[sortby].lastIndexOf(":")+1));
            var min2 = Math.round( row2[sortby].substr(row2[sortby].indexOf(":")+1,2) );
            var hrs2 = Math.round( row2[sortby].substr(0,row2[sortby].indexOf(":")) );
            var r1 = hrs1*3600+min1*60+sec1;
            var r2 = hrs2*3600+min2*60+sec2;
            var r = -ascending;
            if(r1 > r2)
                r = ascending;
            if(r1 == r2)
                r = 0;
            return r;
        },
        
        updateDataCellRenderers : function() {
            var tcm = this.tab.getTableColumnModel();
            var resizeBehavior = tcm.getBehavior();
            for(var i=0; i<this.tabDescription.columns.length; i++) {
                var I = this.tabDescription.columns[i];
                switch(I.type) {
                    case "bool" :
                        tcm.setDataCellRenderer(i, new qx.ui.table.cellrenderer.Boolean());
                        break;
                    case "date" :
                        tcm.setDataCellRenderer(i, new qx.ui.table.cellrenderer.Date());
                        break;
                    case "float":
                        var rnd = new qx.ui.table.cellrenderer.Number();
                        if(I.maximumFractionDigit != undefined) {
                            var fmt = new qx.util.format.NumberFormat();
                            fmt.setMaximumFractionDigits(I.maximumFractionDigit);
                            rnd.setNumberFormat(fmt);
                        }
                        tcm.setDataCellRenderer(i, rnd);
                        break;
                    case "sec":
                        var own = this;
                        this.setSortMethods(i, {
                            "ascending": function(row1, row2) { 
                                return own.mySort(row1, row2, arguments.callee.columnIndex, 1)
                            }, 
                            "descending": function(row1, row2) { 
                                return own.mySort(row1, row2, arguments.callee.columnIndex, -1)
                            }
                        });
                        break;
                }
                if(I.width != undefined)
                    resizeBehavior.setWidth(i, I.width);
            }
        },

        getToolTip : function(column, row) {
            return "";
        },


        onRowDataIncome : function(result) {
            var sortColumnIndex=0;
            var rows = [];
            this.data = {};
            for(var i=0; i < result.values.length; i++) {
                var Row = result.values[i];
                var rData = [];
                for(var j in this.columnsDesc) {
                    var desc = this.columnsDesc[j];
                    if(Row[desc.name] != undefined) {
                        var rowVal = Row[desc.name];
                        switch(desc.type) {
                            case "sec":
                                rowVal = rowVal *1;
                                var hour = Math.floor(rowVal/3600);
                                rowVal = rowVal%3600;
                                var min = Math.floor(rowVal / 60);
                                var sec = (rowVal % 60).toFixed(0);
                                rowVal = bsk.util.utils.formatTime(hour, min, sec);
                                break;
                            case "float":
                                rowVal = rowVal *1;
                                if(desc.tofixed)
                                    rowVal = rowVal.toFixed(desc.tofixed);
                                break;
                            case "erl_datetime":
                                var dt = bsk.util.utils.getDateLocal(rowVal, 0);
                                rowVal = bsk.util.utils.formatJsDateTime(dt);
                                break;

                            case "erl_datetime_utc":
                                var dt = bsk.util.utils.getDate(rowVal, 0);
                                rowVal = bsk.util.utils.formatJsDateTime(dt);
                                break;
                            case "percent":
                                rowVal = rowVal *100;
                                if(desc.tofixed)
                                    rowVal = rowVal.toFixed(desc.tofixed);
                                rowVal += "%";
                                break;
                            case "checkbox":
                                //rowVal = "<span style=\"color:red\">" + rowVal + "</span>";   
                                break;
                            case "enum":
                                if(desc.dict){
                                    var old_rowVal = rowVal
                                    rowVal = desc.dict[old_rowVal];
                                    if(!rowVal)
                                        rowVal = desc.dict['default'] + " (" + old_rowVal + ")";
                                }
                                break;
                            default:
                                break;
                        }
                    }
                    else
                        rowVal = "---";
                    rData.push(rowVal);
                }
                this.data[rData] = Row;
                rows.push(rData);
            }
 //           this.data = result.values;
            this.addRows(rows, 0);
            if(result.sort != undefined && this.columnsDesc[result.sort] != undefined)
                this.sortIndex = this.columnsDesc[result.sort].columnIndex;

            
            if(result.ascending != undefined)
                this.asc = result.ascending;
            this.removeListener("sorted", this.onSortChange, this);
            this.sortByColumn(this.sortIndex, this.asc);
            this.addListener("sorted", this.onSortChange, this);
        },

        clear : function() {
            this.removeRows(0, this.getRowCount());
        }
    }
});


