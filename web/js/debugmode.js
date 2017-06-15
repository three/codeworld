
/*
 * Copyright 2017 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
"use strict";

window.debugMode = false;

function initDebugMode(getStackAtPoint) {
    var canvas = document.getElementById("screen");
    var infobox = document.createElement("div");

    infobox.style.position = "absolute";
    infobox.style.border = "1px solid black";
    infobox.style.background = "white";
    infobox.style.minWidth = "60px";
    infobox.style.padding = "10px";
    infobox.style.display = "none";
    document.body.appendChild(infobox);

    canvas.addEventListener("click", function (evt) {
        if (!debugMode) return;

        var ret = {};
        getStackAtPoint({
            x: evt.clientX,
            y: evt.clientY,
        }, ret);

        if (ret.srcLoc) {
            parent.codeworldEditor.setSelection({
                line: ret.srcLoc.startLine-1,
                ch: ret.srcLoc.startCol-1,
            }, {
                line: ret.srcLoc.endLine-1,
                ch: ret.srcLoc.endCol-1,
            });
            
            var text = ret.srcLoc.startLine + ":" + ret.srcLoc.startCol;
            infobox.innerHTML = "";
            infobox.appendChild(
                document.createTextNode(text)
                );
            infobox.style.left = evt.clientX + "px";
            infobox.style.top  = evt.clientY + "px";
            infobox.style.display = "block";
        } else {
            infobox.style.display = "none";
        }
    });

    canvas.onblur = (function (evt) {
        infobox.style.display = "none";
    });

    window.debugMode = true;
}

function haltDebugMode() {
    window.debugMode = false;
}
