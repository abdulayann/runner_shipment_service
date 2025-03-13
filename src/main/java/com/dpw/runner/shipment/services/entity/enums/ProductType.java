package com.dpw.runner.shipment.services.entity.enums;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
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
    Shipment_Sea_TransShip_BLK(42, "Shipment Sea TransShip BLK"),
    Shipment_Sea_TransShip_FCL(43, "Shipment Sea TransShip FCL"),
    Shipment_Sea_TransShip_LCL(44, "Shipment Sea TransShip LCL"),
    Shipment_Sea_TransShip_Liquid(45, "Shipment Sea TransShip Liquid"),
    Shipment_Sea_TransShip_RORO(46, "Shipment Sea TransShip RORO"),
    Shipment_Sea_ReShip_BBK(47, "Shipment Sea ReShip BBK"),
    Shipment_Sea_ReShip_BLK(48, "Shipment Sea ReShip BLK"),
    Shipment_Sea_ReShip_FCL(49, "Shipment Sea ReShip FCL"),
    Shipment_Sea_ReShip_LCL(50, "Shipment Sea ReShip LCL"),
    Shipment_Sea_ReShip_Liquid(51, "Shipment Sea ReShip Liquid"),
    Shipment_Sea_ReShip_RORO(52, "Shipment Sea ReShip RORO"),
    DC_NOTE(53, "DC NOTE"),
    DC_STATEMENT(54, "DC STATEMENT"),
    All_Shipments(55, "All Shipments"),
    Shipment_Sea_CrossTrade_BBK(56, "Shipment Sea CrossTrade BBK"),
    Shipment_Sea_CrossTrade_BLK(57, "Shipment Sea CrossTrade BLK"),
    Shipment_Sea_CrossTrade_FCL(58, "Shipment Sea CrossTrade FCL"),
    Shipment_Sea_CrossTrade_LCL(59, "Shipment Sea CrossTrade LCL"),
    Shipment_Sea_CrossTrade_LQD(60, "Shipment Sea CrossTrade LQD"),
    Shipment_Sea_CrossTrade_ROR(61, "Shipment Sea CrossTrade ROR"),
    Shipment_Air_CrossTrade_LSE(62, "Shipment Air CrossTrade LSE"),
    Shipment_Air_CrossTrade_ULD(63, "Shipment Air CrossTrade ULD"),
    Shipment_Road_CrossTrade_BBK(64, "Shipment Road CrossTrade BBK"),
    Shipment_Road_CrossTrade_BLK(66, "Shipment Road CrossTrade FCL"),
    Shipment_Road_CrossTrade_FCL(65, "Shipment Road CrossTrade BLK"),
    Shipment_Road_CrossTrade_LCL(67, "Shipment Road CrossTrade LCL"),
    Shipment_Road_CrossTrade_LQD(68, "Shipment Road CrossTrade LQD"),
    Shipment_Road_CrossTrade_ROR(69, "Shipment Road CrossTrade ROR"),
    Shipment_Rail_CrossTrade_BBK(70, "Shipment Rail CrossTrade BBK"),
    Shipment_Rail_CrossTrade_BLK(71, "Shipment Rail CrossTrade BLK"),
    Shipment_Rail_CrossTrade_FCL(72, "Shipment Rail CrossTrade FCL"),
    Shipment_Rail_CrossTrade_LCL(73, "Shipment Rail CrossTrade LCL"),
    Shipment_Rail_CrossTrade_LQD(74, "Shipment Rail CrossTrade LQD"),
    Shipment_Rail_CrossTrade_ROR(75, "Shipment Rail CrossTrade ROR"),
    Shipment_Sea_CrossTrade(76, "Shipment Sea CrossTrade"),
    Shipment_Air_CrossTrade(77, "Shipment Air CrossTrade"),
    Shipment_Road_CrossTrade(78, "Shipment Road CrossTrade"),
    Shipment_Rail_CrossTrade(79, "Shipment Rail CrossTrade"),
    Shipment_Sea_TransShip(80, "Shipment Sea TransShip"),
    Shipment_Air_TransShip(81, "Shipment Air TransShip"),
    Shipment_Road_TransShip(82, "Shipment Road TransShip"),
    Shipment_Rail_TransShip(83, "Shipment Rail TransShip"),
    Container_Management(84, "Container Management"),
    Quote_Road_EXIM(85, "Quote Road EXIM"),
    Quote_Rail_EXIM(86, "Quote Rail EXIM"),
    Common_Sequence(87, "Common Sequence");

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
