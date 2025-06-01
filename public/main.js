const program = "3 + 3";
let memory = null;
let instance = null;

const outputEl = document.getElementById("output");

const importObject = {
    env: {
        write(fd, addr, len) {
            try {
                // read the message from the wasm memory's underyling buffer
                let buff = memory.buffer.slice(addr, addr + len);
                // use TextDecoder to convert the raw buffer into a string
                let str = new TextDecoder().decode(buff);
                switch (fd) {
                    case 1: // STDOUT
                        outputEl.textContent += str;
                        return len;
                    case 2: //STDERR
                        console.error(str); // standard error
                        return len;
                }
                return 0; // in the case that 0 bytes were written
            } catch (err) {
                return -1; // return -1 if an error occurs
            }
        }

    }
};

WebAssembly.instantiateStreaming(fetch("../zig-out/bin/monkey_web.wasm"), importObject).then(
    (obj) => {
        const instance = obj.instance;
        memory = instance.exports.memory;

        const web_main = instance.exports.web_main;
        const alloc = instance.exports.alloc;
        const add = instance.exports.add;


        document.getElementById("runBtn").onclick = () => {
            outputEl.textContent = ""
            const input = document.getElementById("inputBox").value;
            const encoder = new TextEncoder();
            const encoded = encoder.encode(input);

            const ptr = alloc(encoded.length + 1);
            const buf = new Uint8Array(memory.buffer, ptr, encoded.length + 1);
            buf.set(encoded);
            buf[encoded.length] = 0;

            const result = web_main(ptr);
            console.log("wasm exited with code:", result);
        };


    },
);
