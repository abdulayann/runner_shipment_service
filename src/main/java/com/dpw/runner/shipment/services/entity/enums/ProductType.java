package com.dpw.runner.shipment.services.entity.enums;

public enum ProductType {

    E2E_GL(1, "E2E GL"),
    RAIL_DOMESTIC(2, "Rail Domestic"),
    RAIL_EXIM(3, "Rail Exim"),
    FCL_TRUCKING(4, "FCL Trucking"),
    COASTAL_DOMESTIC(5, "Coastal Domestic"),
    DPD(6, "DPD"),
    DOMESTIC_BARGE(7, "Domestic Barge"),
    INTERNATIONAL_BARGE(8, "International Barge"),
    OCEAN(9, "Ocean"),
    TRANSPORT(10, "Transport"),
    PAYMENT(11, "Payment"),
    RECEIPT(12, "Receipt"),
    SHIPMENT_AIR_EXP(13, "Shipment Air EXP"),
    SHIPMENT_AIR_IMP(14, "Shipment Air IMP"),
    SHIPMENT_AIR_EXIM(15, "Shipment Air EXIM"),
    SHIPMENT_SEA_EXP(16, "Shipment Sea EXP"),
    SHIPMENT_SEA_IMP(17, "Shipment Sea IMP"),
    SHIPMENT_SEA_EXIM(18, "Shipment Sea EXIM"),
    QUOTE_SEA_EXIM(19, "Quote Sea EXIM"),
    QUOTE_AIR_EXIM(20, "Quote Air EXIM"),
    SHIPMENT_ROAD_EXP(21, "Shipment Road EXP"),
    CONSOLIDATION_AIR_EXIM(22, "Consolidation Air EXIM"),
    CONSOLIDATION_SEA_EXIM(23, "Consolidation Sea EXIM"),
    CONSOLIDATION_ALL(24, "Consolidation All"),
    BILL_ALL(25, "Bill All"),
    BILL_SEA_EXIM(26, "Bill Sea EXIM"),
    BILL_AIR_EXIM(27, "Bill Air EXIM"),
    TRANSPORT_ALL(28, "Transport All"),
    SHIPMENT_SEA_EXP_BBK(29, "Shipment Sea EXP BBK"),
    SHIPMENT_SEA_EXP_BLK(30, "Shipment Sea EXP BLK"),
    SHIPMENT_SEA_EXP_FCL(31, "Shipment Sea EXP FCL"),
    SHIPMENT_SEA_EXP_LCL(32, "Shipment Sea EXP LCL"),
    SHIPMENT_SEA_EXP_LIQUID(33, "Shipment Sea EXP Liquid"),
    SHIPMENT_SEA_EXP_RORO(34, "Shipment Sea EXP RORO"),
    SHIPMENT_SEA_IMP_BBK(35, "Shipment Sea IMP BBK"),
    SHIPMENT_SEA_IMP_BLK(36, "Shipment Sea IMP BLK"),
    SHIPMENT_SEA_IMP_FCL(37, "Shipment Sea IMP FCL"),
    SHIPMENT_SEA_IMP_LCL(38, "Shipment Sea IMP LCL"),
    SHIPMENT_SEA_IMP_LIQUID(39, "Shipment Sea IMP Liquid"),
    SHIPMENT_SEA_IMP_RORO(40, "Shipment Sea IMP RORO"),
    SHIPMENT_SEA_TRANS_SHIP_BBK(41, "Shipment Sea TransShip BBK"),
    SHIPMENT_SEA_TRANS_SHIP_BLK(42, "Shipment Sea TransShip BLK"),
    SHIPMENT_SEA_TRANS_SHIP_FCL(43, "Shipment Sea TransShip FCL"),
    SHIPMENT_SEA_TRANS_SHIP_LCL(44, "Shipment Sea TransShip LCL"),
    SHIPMENT_SEA_TRANS_SHIP_LIQUID(45, "Shipment Sea TransShip Liquid"),
    SHIPMENT_SEA_TRANS_SHIP_RORO(46, "Shipment Sea TransShip RORO"),
    SHIPMENT_SEA_RE_SHIP_BBK(47, "Shipment Sea ReShip BBK"),
    SHIPMENT_SEA_RE_SHIP_BLK(48, "Shipment Sea ReShip BLK"),
    SHIPMENT_SEA_RE_SHIP_FCL(49, "Shipment Sea ReShip FCL"),
    SHIPMENT_SEA_RE_SHIP_LCL(50, "Shipment Sea ReShip LCL"),
    SHIPMENT_SEA_RE_SHIP_LIQUID(51, "Shipment Sea ReShip Liquid"),
    SHIPMENT_SEA_RE_SHIP_RORO(52, "Shipment Sea ReShip RORO"),
    DC_NOTE(53, "DC NOTE"),
    DC_STATEMENT(54, "DC STATEMENT"),
    ALL_SHIPMENTS(55, "All Shipments"),
    SHIPMENT_SEA_CROSS_TRADE_BBK(56, "Shipment Sea CrossTrade BBK"),
    SHIPMENT_SEA_CROSS_TRADE_BLK(57, "Shipment Sea CrossTrade BLK"),
    SHIPMENT_SEA_CROSS_TRADE_FCL(58, "Shipment Sea CrossTrade FCL"),
    SHIPMENT_SEA_CROSS_TRADE_LCL(59, "Shipment Sea CrossTrade LCL"),
    SHIPMENT_SEA_CROSS_TRADE_LQD(60, "Shipment Sea CrossTrade LQD"),
    SHIPMENT_SEA_CROSS_TRADE_ROR(61, "Shipment Sea CrossTrade ROR"),
    SHIPMENT_AIR_CROSS_TRADE_LSE(62, "Shipment Air CrossTrade LSE"),
    SHIPMENT_AIR_CROSS_TRADE_ULD(63, "Shipment Air CrossTrade ULD"),
    SHIPMENT_ROAD_CROSS_TRADE_BBK(64, "Shipment Road CrossTrade BBK"),
    SHIPMENT_ROAD_CROSS_TRADE_BLK(66, "Shipment Road CrossTrade FCL"),
    SHIPMENT_ROAD_CROSS_TRADE_FCL(65, "Shipment Road CrossTrade BLK"),
    SHIPMENT_ROAD_CROSS_TRADE_LCL(67, "Shipment Road CrossTrade LCL"),
    SHIPMENT_ROAD_CROSS_TRADE_LQD(68, "Shipment Road CrossTrade LQD"),
    SHIPMENT_ROAD_CROSS_TRADE_ROR(69, "Shipment Road CrossTrade ROR"),
    SHIPMENT_RAIL_CROSS_TRADE_BBK(70, "Shipment Rail CrossTrade BBK"),
    SHIPMENT_RAIL_CROSS_TRADE_BLK(71, "Shipment Rail CrossTrade BLK"),
    SHIPMENT_RAIL_CROSS_TRADE_FCL(72, "Shipment Rail CrossTrade FCL"),
    SHIPMENT_RAIL_CROSS_TRADE_LCL(73, "Shipment Rail CrossTrade LCL"),
    SHIPMENT_RAIL_CROSS_TRADE_LQD(74, "Shipment Rail CrossTrade LQD"),
    SHIPMENT_RAIL_CROSS_TRADE_ROR(75, "Shipment Rail CrossTrade ROR"),
    SHIPMENT_SEA_CROSS_TRADE(76, "Shipment Sea CrossTrade"),
    SHIPMENT_AIR_CROSS_TRADE(77, "Shipment Air CrossTrade"),
    SHIPMENT_ROAD_CROSS_TRADE(78, "Shipment Road CrossTrade"),
    SHIPMENT_RAIL_CROSS_TRADE(79, "Shipment Rail CrossTrade"),
    SHIPMENT_SEA_TRANS_SHIP(80, "Shipment Sea TransShip"),
    SHIPMENT_AIR_TRANS_SHIP(81, "Shipment Air TransShip"),
    SHIPMENT_ROAD_TRANS_SHIP(82, "Shipment Road TransShip"),
    SHIPMENT_RAIL_TRANS_SHIP(83, "Shipment Rail TransShip"),
    CONTAINER_MANAGEMENT(84, "Container Management"),
    QUOTE_ROAD_EXIM(85, "Quote Road EXIM"),
    QUOTE_RAIL_EXIM(86, "Quote Rail EXIM"),
    COMMON_SEQUENCE(87, "Common Sequence");

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
