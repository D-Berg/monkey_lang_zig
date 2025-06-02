const program = "3 + 3";
let memory = null;
let instance = null;

const console_element = document.getElementById("console");

// Intercept input and store it for WASM to read
//
const stdin = document.getElementById("stdin");

const importObject = {
    env: {
        js_write(fd, addr, len) {
            try {
                // read the message from the wasm memory's underyling buffer
                let buff = memory.buffer.slice(addr, addr + len);
                // use TextDecoder to convert the raw buffer into a string
                let str = new TextDecoder().decode(buff);
                switch (fd) {
                    case 1: // STDOUT
                        // outputEl.textContent += str;
                        console_element.innerHTML += `<span class="stdout">${str}</span>`;
                        return len;
                    case 2: //STDERR
                        // outputEl.textContent += str;
                        console_element.innerHTML += `<span class="stderr">${str}</span>`;
                        return len;
                }
                return 0; // in the case that 0 bytes were written
            } catch (err) {
                return -1; // return -1 if an error occurs
            }
        },
    }
};

WebAssembly.instantiateStreaming(fetch("../zig-out/bin/monkey_web.wasm"), importObject).then(
    (obj) => {
        const instance = obj.instance;
        memory = instance.exports.memory;

        const eval = instance.exports.wasm_evaluate;
        const alloc = instance.exports.alloc;
        const free = instance.exports.free;

        stdin.addEventListener('keydown', (e) => {
            if (e.key == "Enter") {
                console.log("pressed enter")
                // console_element.innerHTML = ""

                const input = stdin.value;
                console.log("stdin = ", input);
                const encoder = new TextEncoder();
                const encoded = encoder.encode(input);

                const len = encoded.length
                const ptr = alloc(len);
                const buf = new Uint8Array(memory.buffer, ptr, len);
                buf.set(encoded);
                // buf[encoded.length] = 0;

                const result = eval(ptr, len);

                free(ptr, len);

                stdin.value = "";
                console.log("wasm exited with code:", result);

            }
        });


    },
);
