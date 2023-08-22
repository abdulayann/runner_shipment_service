package com.dpw.runner.shipment.services.entity.enums;

public enum ProductType {

    E2E_GL(1, "E2E GL"),
    Rail_Domestic(2, "Rail Domestic"),
    Rail_Exim(3, "Rail Exim"),
    FCL_Trucking(4, "FCL Trucking"),
    Coastal_Domestic(5, "Coastal Domestic"),
    DPD(6, "DPD"),
    Domestic_Barge(7, "Domestic Barge"),
    International_Barge(8, "International Barge"),
    Ocean(9, "Ocean"),
    Transport(10, "Transport"),
    Payment(11, "Payment"),
    Receipt(12, "Receipt"),
    Shipment_Air_EXP(13, "Shipment Air EXP"),
    Shipment_Air_IMP(14, "Shipment Air IMP"),
    Shipment_Air_EXIM(15, "Shipment Air EXIM"),
    Shipment_Sea_EXP(16, "Shipment Sea EXP"),
    Shipment_Sea_IMP(17, "Shipment Sea IMP"),
    Shipment_Sea_EXIM(18, "Shipment Sea EXIM"),
    Quote_Sea_EXIM(19, "Quote Sea EXIM"),
    Quote_Air_EXIM(20, "Quote Air EXIM"),
    Shipment_Road_EXP(21, "Shipment Road EXP"),
    Consolidation_Air_EXIM(22, "Consolidation Air EXIM"),
    Consolidation_Sea_EXIM(23, "Consolidation Sea EXIM"),
    Consolidation_All(24, "Consolidation All"),
    Bill_All(25, "Bill All"),
    Bill_Sea_EXIM(26, "Bill Sea EXIM"),
    Bill_Air_EXIM(27, "Bill Air EXIM"),
    Transport_All(28, "Transport All"),
    Shipment_Sea_EXP_BBK(29, "Shipment Sea EXP BBK"),
    Shipment_Sea_EXP_BLK(30, "Shipment Sea EXP BLK"),
    Shipment_Sea_EXP_FCL(31, "Shipment Sea EXP FCL"),
    Shipment_Sea_EXP_LCL(32, "Shipment Sea EXP LCL"),
    Shipment_Sea_EXP_Liquid(33, "Shipment Sea EXP Liquid"),
    Shipment_Sea_EXP_RORO(34, "Shipment Sea EXP RORO"),
    Shipment_Sea_IMP_BBK(35, "Shipment Sea IMP BBK"),
    Shipment_Sea_IMP_BLK(36, "Shipment Sea IMP BLK"),
    Shipment_Sea_IMP_FCL(37, "Shipment Sea IMP FCL"),
    Shipment_Sea_IMP_LCL(38, "Shipment Sea IMP LCL"),
    Shipment_Sea_IMP_Liquid(39, "Shipment Sea IMP Liquid"),
    Shipment_Sea_IMP_RORO(40, "Shipment Sea IMP RORO"),
    Shipment_Sea_TransShip_BBK(41, "Shipment Sea TransShip BBK"),
    Shipment_Sea_TransShip_BLK(42, "Shipment Sea TransShip BLK");
//    SHIPMENT_SEA_TRANSSHIP_FCL(43, "Shipment Sea TransShip FCL"),
//    SHIPMENT_SEA_TRANSSHIP_LCL(44, "Shipment Sea TransShip LCL"),
//    SHIPMENT_SEA_TRANSSHIP_Liquid(45, "Shipment Sea TransShip Liquid"),
//    SHIPMENT_SEA_TRANSSHIP_RORO(46, "Shipment Sea TransShip RORO"),
//    SHIPMENT_SEA_RESHIP_BBK(47, "Shipment Sea ReShip BBK"),
//    SHIPMENT_SEA_RESHIP_BLK(48, "Shipment Sea ReShip BLK"),
//    SHIPMENT_SEA_RESHIP_FCL(49, "Shipment Sea ReShip FCL"),
//    SHIPMENT_SEA_RESHIP_LCL(50, "Shipment Sea ReShip LCL"),
//    SHIPMENT_SEA_RESHIP_Liquid(51, "Shipment Sea ReShip Liquid"),
//    SHIPMENT_SEA_RESHIP_RORO(52, "Shipment Sea ReShip RORO"),
//    DC_NOTE(53, "DC NOTE"),
//    DC_STATEMENT(54, "DC STATEMENT"),
//    ALL_SHIPMENTS(55, "All Shipments"),
//    SHIPMENT_SEA_CROSS_TRADE_BBK(56, "Shipment Sea CrossTrade BBK"),
//    SHIPMENT_SEA_CROSS_TRADE_BLK(57, "Shipment Sea CrossTrade BLK"),
//    SHIPMENT_SEA_CROSS_TRADE_FCL(58, "Shipment Sea CrossTrade FCL"),
//    SHIPMENT_SEA_CROSS_TRADE_LCL(59, "Shipment Sea CrossTrade LCL"),
//    SHIPMENT_SEA_CROSS_TRADE_LQD(60, "Shipment Sea CrossTrade LQD"),
//    SHIPMENT_SEA_CROSS_TRADE_ROR(61, "Shipment Sea CrossTrade ROR"),
//    SHIPMENT_AIR_CROSS_TRADE_LSE(62, "Shipment Air CrossTrade LSE"),
//    SHIPMENT_AIR_CROSS_TRADE_ULD(63, "Shipment Air CrossTrade ULD"),
//    SHIPMENT_ROAD_CROSS_TRADE_BBK(64, "Shipment Road CrossTrade BBK"),
//    SHIPMENT_ROAD_CROSS_TRADE_BLK(65, "Shipment Road CrossTrade BLK"),
//    SHIPMENT_ROAD_CROSS_TRADE_FCL(66, "Shipment Road CrossTrade FCL"),
//    SHIPMENT_ROAD_CROSS_TRADE_LCL(67, "Shipment Road CrossTrade LCL"),
//    SHIPMENT_ROAD_CROSS_TRADE_LQD(68, "Shipment Road CrossTrade LQD"),
//    SHIPMENT_ROAD_CROSS_TRADE_ROR(69, "Shipment Road CrossTrade ROR"),
//    SHIPMENT_RAIL_CROSS_TRADE_BBK(70, "Shipment Rail CrossTrade BBK"),
//    SHIPMENT_RAIL_CROSS_TRADE_BLK(71, "Shipment Rail CrossTrade BLK"),
//    SHIPMENT_RAIL_CROSS_TRADE_FCL(72, "Shipment Rail CrossTrade FCL"),
//    SHIPMENT_RAIL_CROSS_TRADE_LCL(73, "Shipment Rail CrossTrade LCL"),
//    SHIPMENT_RAIL_CROSS_TRADE_LQD(74, "Shipment Rail CrossTrade LQD"),
//    SHIPMENT_RAIL_CROSS_TRADE_ROR(75, "Shipment Rail CrossTrade ROR"),
//    SHIPMENT_SEA_CROSS_TRADE(76, "Shipment Sea CrossTrade"),
//    SHIPMENT_AIR_CROSS_TRADE(77, "Shipment Air CrossTrade"),
//    SHIPMENT_ROAD_CROSS_TRADE(78, "Shipment Road CrossTrade"),
//    SHIPMENT_RAIL_CROSS_TRADE(79, "Shipment Rail CrossTrade"),
//    SHIPMENT_SEA_TRANSSHIP(80, "Shipment Sea TransShip"),
//    SHIPMENT_AIR_TRANSSHIP(81, "Shipment Air TransShip"),
//    SHIPMENT_ROAD_TRANSSHIP(82, "Shipment Road TransShip"),
//    SHIPMENT_RAIL_TRANSSHIP(83, "Shipment Rail TransShip"),
//    CONTAINER_MANAGEMENT(84, "Container Management");

    private final int value;
    private final String description;

    ProductType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }
}
