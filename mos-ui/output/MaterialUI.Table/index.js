// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Record_Class = require("../Data.Record.Class");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Padding = function (x) {
    return x;
};
var Direction = function (x) {
    return x;
};
var tableSortLabel = function (dictSubrow) {
    return function (p) {
        return React.createElement($foreign.tableSortLabelImpl)(p)([  ]);
    };
};
var tableRow = function (dictSubrow) {
    return React.createElement($foreign.tableRowImpl);
};
var tablePagination = function (dictSubrow) {
    return function (p) {
        return React.createElement($foreign.tablePaginationImpl)(p)([  ]);
    };
};
var tableHead = function (dictSubrow) {
    return React.createElement($foreign.tableHeadImpl);
};
var tableFooter = function (dictSubrow) {
    return React.createElement($foreign.tableFooterImpl);
};
var tableCell = function (dictSubrow) {
    return function (p) {
        return function ($16) {
            return React.createElement($foreign.tableCellImpl)(p)($16);
        };
    };
};
var tableBody = function (dictSubrow) {
    return React.createElement($foreign.tableBodyImpl);
};
var table = function (dictSubrow) {
    return React.createElement($foreign.tableImpl);
};
var none = "none";
var desc = "desc";
var dense = "dense";
var $$default = "default";
var createClassesSortLabel = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClassesRow = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClassesPagination = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClassesHead = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClassesFooter = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClassesCell = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClassesBody = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var checkbox = "checkbox";
var asc = "asc";
module.exports = {
    table: table,
    createClasses: createClasses,
    tableBody: tableBody,
    createClassesBody: createClassesBody,
    tableCell: tableCell,
    createClassesCell: createClassesCell,
    tableHead: tableHead,
    createClassesHead: createClassesHead,
    tableRow: tableRow,
    createClassesRow: createClassesRow,
    tableFooter: tableFooter,
    createClassesFooter: createClassesFooter,
    tablePagination: tablePagination,
    createClassesPagination: createClassesPagination,
    tableSortLabel: tableSortLabel,
    createClassesSortLabel: createClassesSortLabel,
    checkbox: checkbox,
    dense: dense,
    none: none,
    asc: asc,
    desc: desc
};