const CallContract = import("./dist/bundle.js");

/**
 * Exists temporarily for testing purposes. Returns given argument.
 */
exports.callMarketPlaceBuyTest = async (str) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuyTest(str)();
};

/**
 * Calls Seabug Contract 'marketPlaceBuy'.
 * It returns a promise holding no data.
 *
 */
exports.callMarketPlaceBuy = async (config, args) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuy(config)(args)();
};

/**
 * Calls Seabug Contract 'marketPlaceListNft'.
 * Returns a promise holding nft listings.
 *
 */
exports.callMarketPlaceListNft = async (config) => {
    const CC = await CallContract;
    return CC.callMarketPlaceListNft(config)();
};
