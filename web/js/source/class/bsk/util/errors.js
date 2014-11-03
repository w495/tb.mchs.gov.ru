

qx.Class.define("bsk.util.errors",
{
    statics : {
         process : function(own, resp) {
            if(resp.REDIRECT != undefined) {
                window.location=resp.REDIRECT;
                return false;
            }
            if(resp.RELOAD != undefined) {
                location.reload();
                return false;
            }
            if(resp.PERM_REQUIRED != undefined) {
                alert("Не хватает прав доступа. Обратиесь к администратору.");
                return false;
            }
            if(resp.ERROR != undefined) {
                own.show_error(resp.ERROR.type, resp.ERROR.info);
                return false;
            }
            return true;

        }
    }
});
