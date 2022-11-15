export const _clearInputValue = (el) => () => {
    try {
        el.value = "";
    } catch (_) {}
};
