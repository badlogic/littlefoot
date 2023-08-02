import "./visualizer.css";

const INDICATOR_EXPAND = "▶";
const INDICATOR_COLLAPSE = "▼";
const INDICATOR_LEAF = " ";

enum ValueType {
  undefined,
  null,
  boolean,
  number,
  string,
  function,
  array,
  object,
}

type Value = { str: string; type: ValueType };

function isLeaf(value: any) {
  if (typeof value === "undefined") return true;
  if (value === null) return true;
  if (typeof value === "boolean") return true;
  if (typeof value === "number") return true;
  if (typeof value === "string") return true;
  if (typeof value === "function") return true;
  return false;
}

function toValue(value: any, preview: (value: any) => any | undefined): Value {
  const previewText = preview(value);
  if (typeof value === "undefined") return { str: previewText ?? "undefined", type: ValueType.undefined };
  if (value === null) return { str: previewText ?? "null", type: ValueType.null };
  if (typeof value === "boolean") return { str: previewText ?? value.toString(), type: ValueType.boolean };
  if (typeof value === "number") return { str: previewText ?? value.toString(), type: ValueType.number };
  if (typeof value === "string") return { str: previewText ?? JSON.stringify(value), type: ValueType.string };
  if (typeof value === "function") return { str: previewText ?? "function()", type: ValueType.function };
  if (Array.isArray(value)) return { str: previewText ?? `Array(${value.length})`, type: ValueType.array };
  if (typeof value === "object") return { str: previewText ?? "{...}", type: ValueType.object };
  throw new Error("Unsupported value type.");
}

export class Visualizer {
  constructor(
    container: HTMLElement,
    data: any,
    rootName: string,
    shouldExpand: (path: string[], key: string, value: any) => boolean,
    keys: (obj: any) => string[],
    replace: (obj: any, key: string, value: any) => any | undefined,
    preview: (value: any) => any | undefined
  ) {
    const root = document.createElement("div");
    root.classList.add("visualizer");
    container.innerHTML = "";
    container.appendChild(root);
    this.render(rootName, data, null, [], root, shouldExpand, keys, replace, preview);
  }

  renderSpan(clazz: string, text: string) {
    const span = document.createElement("span");
    span.classList.add(clazz);
    span.innerText = text;
    return span;
  }

  renderNode(indicator: string, key: string, value: Value | null, level: number) {
    const node = document.createElement("div");
    node.classList.add("visualizer-node");
    node.style.paddingLeft = level + "em";
    node.setAttribute("data-visualizer-level", level.toString());
    node.appendChild(this.renderSpan("visualizer-node-indicator", indicator));
    node.appendChild(this.renderSpan("visualizer-node-key", key));
    node.appendChild(this.renderSpan("visualizer-node-colon", ":"));
    if (value != null) {
      const valueSpan = this.renderSpan("visualizer-node-value", value.str);
      switch (value.type) {
        case ValueType.undefined:
          valueSpan.classList.add("visualizer-node-undefined");
          break;
        case ValueType.null:
          valueSpan.classList.add("visualizer-node-null");
          break;
        case ValueType.boolean:
          valueSpan.classList.add("visualizer-node-boolean");
          break;
        case ValueType.number:
          valueSpan.classList.add("visualizer-node-number");
          break;
        case ValueType.string:
          valueSpan.classList.add("visualizer-node-string");
          break;
        case ValueType.function:
          valueSpan.classList.add("visualizer-node-function");
          break;
        case ValueType.array:
          valueSpan.classList.add("visualizer-node-array");
          break;
        case ValueType.object:
          valueSpan.classList.add("visualizer-node-object");
          break;
      }
      node.appendChild(valueSpan);
    }
    return node;
  }

  render(
    key: string,
    value: any,
    obj: any,
    path: string[],
    root: HTMLElement,
    shouldExpand: (path: string[], key: string, value: any) => boolean,
    keys: (obj: any) => string[],
    replace: (obj: any, key: string, value: any) => any | undefined,
    preview: (value: any) => any | undefined
  ): HTMLElement {
    path.push(key);
    value = replace(obj, key, value);
    let node: HTMLElement | null = null;
    let indicator: string;
    if (!shouldExpand(path, key, value)) {
      indicator = isLeaf(value) ? INDICATOR_LEAF : INDICATOR_EXPAND;
      node = this.renderNode(indicator, key, toValue(value, preview), path.length);
      root.appendChild(node);
    } else {
      if (isLeaf(value)) {
        indicator = INDICATOR_LEAF;
        node = this.renderNode(INDICATOR_LEAF, key, toValue(value, preview), path.length);
        root.appendChild(node);
      } else {
        if (Array.isArray(value)) {
          indicator = value.length == 0 ? INDICATOR_LEAF : INDICATOR_COLLAPSE;
          node = this.renderNode(indicator, key, value.length == 0 ? { str: "[]", type: ValueType.array } : null, path.length);
          root.appendChild(node);

          if (value.length > 0) {
            const children = new Array(value.length);
            for (let i = 0; i < value.length; i++) {
              children[i] = this.render(i.toString(), value[i], value, path, root, shouldExpand, keys, replace, preview);
            }
          }
        } else {
          const valueKeys = keys(value);
          indicator = valueKeys.length == 0 ? INDICATOR_LEAF : INDICATOR_COLLAPSE;
          node = this.renderNode(indicator, key, valueKeys.length == 0 ? { str: "{}", type: ValueType.object } : null, path.length);
          root.appendChild(node);

          if (valueKeys.length > 0) {
            const children = new Array(value.length);
            let i = 0;
            for (const key of valueKeys) {
              children[i++] = this.render(key, value[key], value, path, root, shouldExpand, keys, replace, preview);
            }
          }
        }
      }
    }

    if (indicator == INDICATOR_COLLAPSE) {
      const nodePath = [...path];
      const nodeLevel = nodePath.length;
      nodePath.pop();
      node.addEventListener("click", () => {
        const tempRoot = document.createElement("div");
        const newNode = this.render(key, value, obj, nodePath, tempRoot, (p) => false, keys, replace, preview);
        let nodeIndex = -1;
        for (let i = 0; i < node!.parentElement!.children.length; i++) {
          if (node!.parentElement!.children[i] == node) {
            nodeIndex = i;
            break;
          }
        }
        if (nodeIndex > -1) {
          while (true) {
            const child = node!.parentElement!.children[nodeIndex + 1];
            if (!child) break;
            const childLevel = Number.parseInt(child.getAttribute("data-visualizer-level")!);
            if (childLevel > nodeLevel) {
              child.remove();
            } else {
              break;
            }
          }
        }
        node?.parentElement?.insertBefore(newNode, node);
        node?.remove();
      });
    }
    if (indicator == INDICATOR_EXPAND) {
      const nodePath = [...path];
      nodePath.pop();
      const nodePathLength = nodePath.length;
      node.addEventListener("click", () => {
        const tempRoot = document.createElement("div");
        this.render(
          key,
          value,
          obj,
          nodePath,
          tempRoot,
          (p) => {
            return p.length <= nodePathLength + 1;
          },
          keys,
          replace,
          preview
        );
        while (tempRoot.children.length > 0) {
          node?.parentElement?.insertBefore(tempRoot.children[0], node);
        }
        node?.remove();
      });
    }

    if (indicator == INDICATOR_LEAF) {
      node.classList.add("visualizer-leaf");
    }

    path.pop();
    return node;
  }
}
