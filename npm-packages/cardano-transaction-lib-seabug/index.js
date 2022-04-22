const seabug = import("./dist/bundle.js");

/**
 * Exists temporarily for testing purposes. Returns given argument.
 */
exports.callMarketPlaceBuyTest = async (str) => {
    const sb = await seabug;
    return sb.callMarketPlaceBuyTest(str)();
};

/**
 * Calls Seabug Contract 'marketPlaceBuy'.
 * It returns a promise holding no data.
 *
 */
exports.callMarketPlaceBuy = async (config, args) => {
    const sb = await seabug;
    return sb.callMarketPlaceBuy(config)(args)();
};

/**
 * Calls Seabug Contract 'marketPlaceListNft'.
 * Returns a promise holding nft listings.
 *
 */
exports.callMarketPlaceListNft = async (config) => {
    const sb = await seabug;
    return sb.callMarketPlaceListNft(config)();
};

/**
 * Connects to Nami wallet.
 * Returns a promise holding the Nami wallet object.
 */
 exports.connectWallet = async () => {
    const sb = await seabug;
    return sb.connectWallet();
};

/**
 * Returns a promise holding the wallet balance.
 */
exports.getWalletBalance = async () => {
    const sb = await seabug;
    return sb.getWalletBalance();
}