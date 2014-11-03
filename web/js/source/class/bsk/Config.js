

/**
    Моделе-зависимые статические функции
**/

qx.Class.define("bsk.Config",
{
    type : "static",

    statics : {

        DOC_FORM_HEIGHT     : 250,
        DOC_FORM_WIDTH      : Math.floor(window.innerWidth * 0.7) ,

        TEST_FORM_HEIGHT     : 50,
        TEST_FORM_WIDTH      : Math.floor(window.innerWidth * 0.7) ,

        
        DOC_NAME_MAX_LEN    : 500,
        DOC_CONT_MAX_LEN    : 2147483647

    }
});
