/* ************************************************************************
#asset(qx/icon/Tango/16/apps/office-project.png)
#asset(qx/icon/Tango/16/apps/office-calendar.png)
#asset(qx/icon/Tango/16/apps/office-chart.png)
#asset(qx/icon/Tango/16/apps/utilities-calculator.png)
#asset(qx/icon/Tango/16/apps/utilities-dictionary.png)
#asset(qx/icon/Tango/16/apps/utilities-statistics.png)
#asset(qx/icon/Tango/16/categories/system.png)
************************************************************************ */

qx.Class.define("bsk.view.NavTree",
{
    extend : qx.ui.tree.Tree,
 
    construct : function(root) {
        this.biz = root;
        this.base(arguments);
        this.setHideRoot(true);
        this.setOpenMode("click");
//        this.setFont(new qx.bom.Font(16));
        this.addListener("changeSelection", this._onMenuSelect, this);

        this._buildMenuTree();
    },

    members : {

        _onMenuSelect : function(e) {
            var I = this.getSelection()[0];
            var L = I.getLabel();
            if(this.menu[L] != undefined)
                this.biz.onMenuChange(this.menu[L]);//this.menu[L].tab, this.menu[L].control);
        },

        _buildMenuTree : function() {
            var req = new qx.io.remote.Request('resource/bsk/descr/menu.json', "GET", "application/json");
            //var req = new qx.io.remote.Request('/js/source/resource/bsk/descr/menu.json', "GET", "application/json");
            req.addListener("completed", this._onGetMenuResource, this);
            req.send();
        },

        _onGetMenuResource : function(response) {
            var result = response.getContent();
            if (bsk.util.errors.process(this, result)==false) return false;
            this.buildMenu(result);
        },
        
        buildMenu : function(menu) {
            var root = new qx.ui.tree.TreeFolder("root");
            root.setOpen(true);
            this.setRoot(root);
            this.menu = {};
            for(var i=0; i<menu.length; i++) {
                var I = menu[i];
                var F = new qx.ui.tree.TreeFolder(I.name);
                if(I.icon != undefined)
                    F.setIcon(I.icon);
                F.setOpen(I.opened == true)

                root.add(F);
                if(I.subitems != undefined) {
                    for(var j=0; j<I.subitems.length; j++) {
                        var M = I.subitems[j];
                        var E = new qx.ui.tree.TreeFile(M.name);
                        this.menu[M.name] = M;
                        E.setIcon("icon/16/categories/system.png");
                        F.add(E);
                    }
                }
            }
            this.biz.hide_global_pb();
        },

        _onR1 : function(e) {
        }
    }
});


