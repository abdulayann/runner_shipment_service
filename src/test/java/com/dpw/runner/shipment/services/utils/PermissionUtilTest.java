package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@Execution(CONCURRENT)
class PermissionUtilTest {

    @BeforeEach
    void setUp() {
    }

    @Test
    void testGenerateFilterCriteriaFromPermissions_massShipment() {
        ArrayList<String> permissions = new ArrayList<>(Arrays.asList(
                "Shipments:List:Air Shipment:ImportAirShipmentList",
                "Shipments:List:Air Shipment:ExportAirShipmentList",
                "Shipments:List:Air Shipment:ReshipmentAirShipmentList",
                "Shipments:List:Air Shipment:TranshipmentAirShipmentList",
                "Shipments:List:Air Shipment:EmptyContainerAirShipmentList",
                "Shipments:List:Air Shipment:DomesticAirShipmentList",
                "Shipments:List:Air Shipment:CrossTradeAirShipmentList",
                "Shipments:List:Air Shipment:AllAirShipmentList",
                "Shipments:List:All Shipment:ImportAllShipmentList",
                "Shipments:List:All Shipment:ExportAllShipmentList",
                "Shipments:List:All Shipment:ReshipmentAllShipmentList",
                "Shipments:List:All Shipment:TranshipmentAllShipmentList",
                "Shipments:List:All Shipment:EmptyContainerAllShipmentList",
                "Shipments:List:All Shipment:DomesticAllShipmentList",
                "Shipments:List:All Shipment:CrossTradeAllShipmentList",
                "Shipments:List:All Shipment:AllShipmentList",
                "Shipments:List:Rail Shipment:ImportRailShipmentList",
                "Shipments:List:Rail Shipment:ExportRailShipmentList",
                "Shipments:List:Rail Shipment:ReshipmentRailShipmentList",
                "Shipments:List:Rail Shipment:TranshipmentRailShipmentList",
                "Shipments:List:Rail Shipment:EmptyContainerRailShipmentList",
                "Shipments:List:Rail Shipment:DomesticRailShipmentList",
                "Shipments:List:Rail Shipment:CrossTradeRailShipmentList",
                "Shipments:List:Rail Shipment:AllRailShipmentList",
                "Shipments:List:Road Shipment:ImportRoadShipmentList",
                "Shipments:List:Road Shipment:ExportRoadShipmentList",
                "Shipments:List:Road Shipment:ReshipmentRoadShipmentList",
                "Shipments:List:Road Shipment:TranshipmentRoadShipmentList",
                "Shipments:List:Road Shipment:EmptyContainerRoadShipmentList",
                "Shipments:List:Road Shipment:DomesticRoadShipmentList",
                "Shipments:List:Road Shipment:CrossTradeRoadShipmentList",
                "Shipments:List:Road Shipment:AllRoadShipmentList",
                "Consolidations:Update:Air Consolidation:ImportAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:ExportAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:ReshipmentAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:TranshipmentAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:EmptyContainerAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:DomesticAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:CrossTradeAirConsolidationUpdate",
                "Consolidations:Update:Air Consolidation:AllAirConsolidationUpdate",
                "Consolidations:Update:All Consolidation:ImportAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:ExportAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:ReshipmentAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:TranshipmentAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:EmptyContainerAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:DomesticAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:CrossTradeAllConsolidationUpdate",
                "Consolidations:Update:All Consolidation:AllConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:ImportRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:ExportRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:ReshipmentRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:TranshipmentRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:EmptyContainerRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:DomesticRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:CrossTradeRailConsolidationUpdate",
                "Consolidations:Update:Rail Consolidation:AllRailConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:ImportRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:ExportRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:ReshipmentRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:TranshipmentRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:EmptyContainerRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:DomesticRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:CrossTradeRoadConsolidationUpdate",
                "Consolidations:Update:Road Consolidation:AllRoadConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:ImportSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:ExportSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:ReshipmentSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:TranshipmentSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:EmptyContainerSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:DomesticSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:CrossTradeSeaConsolidationUpdate",
                "Consolidations:Update:Sea Consolidation:AllSeaConsolidationUpdate",
                "Consolidations:Retrive:Air Consolidation:ImportAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:ExportAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:ReshipmentAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:TranshipmentAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:EmptyContainerAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:DomesticAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:CrossTradeAirConsolidationRetrive",
                "Consolidations:Retrive:Air Consolidation:AllAirConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:ImportAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:ExportAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:ReshipmentAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:TranshipmentAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:EmptyContainerAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:DomesticAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:CrossTradeAllConsolidationRetrive",
                "Consolidations:Retrive:All Consolidation:AllConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:ImportRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:ExportRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:ReshipmentRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:TranshipmentRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:EmptyContainerRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:DomesticRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:CrossTradeRailConsolidationRetrive",
                "Consolidations:Retrive:Rail Consolidation:AllRailConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:ImportRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:ExportRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:ReshipmentRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:TranshipmentRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:EmptyContainerRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:DomesticRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:CrossTradeRoadConsolidationRetrive",
                "Consolidations:Retrive:Road Consolidation:AllRoadConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:ImportSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:ExportSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:ReshipmentSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:TranshipmentSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:EmptyContainerSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:DomesticSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:CrossTradeSeaConsolidationRetrive",
                "Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive",
                "Consolidations:Creation:Air Consolidation:ImportAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:ExportAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:ReshipmentAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:TranshipmentAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:EmptyContainerAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:DomesticAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:CrossTradeAirConsolidationCreate",
                "Consolidations:Creation:Air Consolidation:AllAirConsolidationCreate",
                "Consolidations:Creation:All Consolidation:ImportAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:ExportAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:ReshipmentAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:TranshipmentAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:EmptyContainerAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:DomesticAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:CrossTradeAllConsolidationCreate",
                "Consolidations:Creation:All Consolidation:AllConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:ImportRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:ExportRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:ReshipmentRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:TranshipmentRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:EmptyContainerRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:DomesticRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:CrossTradeRailConsolidationCreate",
                "Consolidations:Creation:Rail Consolidation:AllRailConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:ImportRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:ExportRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:ReshipmentRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:TranshipmentRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:EmptyContainerRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:DomesticRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:CrossTradeRoadConsolidationCreate",
                "Consolidations:Creation:Road Consolidation:AllRoadConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:ImportSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:ExportSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:ReshipmentSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:TranshipmentSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:EmptyContainerSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:DomesticSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:CrossTradeSeaConsolidationCreate",
                "Consolidations:Creation:Sea Consolidation:AllSeaConsolidationCreate",
                "Shipments:List:Sea International Shipment:ImportSeaShipmentList", "sea-imp-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:ExportSeaShipmentList", "sea-exp-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:ReshipmentSeaShipmentList", "sea-res-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:TranshipmentSeaShipmentList", "sea-tra-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:EmptyContainerSeaShipmentList", "sea-emt-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:DomesticSeaShipmentList", "sea-dom-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:CrossTradeSeaShipmentList", "sea-cts-all-international-shipmentList",
                "Shipments:List:Sea International Shipment:AllSeaShipmentList", "sea-all-all-international-shipmentList",
                "Shipments:List:Sea Domestic Shipment:ImportDomesticSeaShipmentList", "sea-imp-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:ExportDomesticSeaShipmentList", "sea-exp-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:ReshipmentDomesticSeaShipmentList", "sea-res-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:TranshipmentDomesticSeaShipmentList", "sea-tra-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:EmptyContainerDomesticSeaShipmentList", "sea-emt-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:DomesticDomesticSeaShipmentList", "sea-dom-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:CrossTradeDomesticSeaShipmentList", "sea-cts-all-domestic-shipmentList",
                "Shipments:List:Sea Domestic Shipment:AllDomesticSeaShipmentList", "sea-all-all-domestic-shipmentList",
                "Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive", "air-imp-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:ExportAirShipmentRetrive", "air-exp-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:ReshipmentAirShipmentRetrive", "air-res-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:TranshipmentAirShipmentRetrive", "air-tra-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:EmptyContainerAirShipmentRetrive", "air-cts-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:DomesticAirShipmentRetrive", "air-dom-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:CrossTradeAirShipmentRetrive", "air-cts-all-shipmentRetrieve",
                "Shipments:Retrive:Air Shipment:AllAirShipmentRetrive", "air-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:ImportAllShipmentRetrive", "all-imp-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:ExportAllShipmentRetrive", "all-exp-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:ReshipmentAllShipmentRetrive", "all-res-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:TranshipmentAllShipmentRetrive", "all-tra-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:EmptyContainerAllShipmentRetrive", "all-emt-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:DomesticAllShipmentRetrive", "all-dom-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:CrossTradeAllShipmentRetrive", "all-cts-all-shipmentRetrieve",
                "Shipments:Retrive:All Shipment:AllShipmentRetrive", "all-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:ImportRailShipmentRetrive", "rai-imp-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:ExportRailShipmentRetrive", "rai-exp-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:ReshipmentRailShipmentRetrive", "rai-res-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:TranshipmentRailShipmentRetrive", "rai-tra-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:EmptyContainerRailShipmentRetrive", "rai-emt-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:DomesticRailShipmentRetrive", "rai-dom-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:CrossTradeRailShipmentRetrive", "rai-cts-shipmentRetrieve",
                "Shipments:Retrive:Rail Shipment:AllRailShipmentRetrive", "rai-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:ImportRoadShipmentRetrive", "roa-imp-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:ExportRoadShipmentRetrive", "roa-exp-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:ReshipmentRoadShipmentRetrive", "roa-res-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:TranshipmentRoadShipmentRetrive", "roa-tra-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:EmptyContainerRoadShipmentRetrive", "roa-emt-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:DomesticRoadShipmentRetrive", "roa-dom-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:CrossTradeRoadShipmentRetrive", "roa-cts-shipmentRetrieve",
                "Shipments:Retrive:Road Shipment:AllRoadShipmentRetrive", "roa-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:ImportSeaShipmentRetrive", "sea-imp-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:ExportSeaShipmentRetrive", "sea-exp-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:ReshipmentSeaShipmentRetrive", "sea-res-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:TranshipmentSeaShipmentRetrive", "sea-tra-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:EmptyContainerSeaShipmentRetrive", "sea-emt-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:DomesticSeaShipmentRetrive", "sea-dom-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:CrossTradeSeaShipmentRetrive", "sea-cts-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea International Shipment:AllSeaShipmentRetrive", "sea-all-all-international-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:ImportDomesticSeaShipmentRetrive", "sea-imp-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:ExportDomesticSeaShipmentRetrive", "sea-exp-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:ReshipmentDomesticSeaShipmentRetrive", "sea-res-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:TranshipmentDomesticSeaShipmentRetrive", "sea-tra-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:EmptyContainerDomesticSeaShipmentRetrive", "sea-emt-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:DomesticDomesticSeaShipmentRetrive", "sea-dom-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:CrossTradeDomesticSeaShipmentRetrive", "sea-cts-all-domestic-shipmentRetrieve",
                "Shipments:Retrive:Sea Domestic Shipment:AllDomesticSeaShipmentRetrive", "sea-all-all-domestic-shipmentRetrieve",
                "Consolidations:List:Sea Consolidation:AllSeaConsolidationList", "sea-consolidationList",
                "Consolidations:List:Sea Consolidation:CrossTradeSeaConsolidationList", "sea-cts-consolidationList",
                "Consolidations:List:Sea Consolidation:DomesticSeaConsolidationList", "sea-dom-consolidationList",
                "Consolidations:List:Sea Consolidation:EmptyContainerSeaConsolidationList", "sea-emt-consolidationList",
                "Consolidations:List:Sea Consolidation:TranshipmentSeaConsolidationList", "sea-tra-consolidationList",
                "Consolidations:List:Sea Consolidation:ReshipmentSeaConsolidationList", "sea-res-consolidationList",
                "Consolidations:List:Sea Consolidation:ExportSeaConsolidationList", "sea-exp-consolidationList",
                "Consolidations:List:Sea Consolidation:ImportSeaConsolidationList", "sea-imp-consolidationList",
                "Consolidations:List:Road Consolidation:AllRoadConsolidationList", "roa-consolidationList",
                "Consolidations:List:Road Consolidation:CrossTradeRoadConsolidationList", "roa-cts-consolidationList",
                "Consolidations:List:All Consolidation:AllConsolidationList", "all-consolidationList",
                "Consolidations:List:Rail Consolidation:ImportRailConsolidationList", "rai-imp-consolidationList",
                "Consolidations:List:Rail Consolidation:ExportRailConsolidationList", "rai-exp-consolidationList",
                "Consolidations:List:Rail Consolidation:ReshipmentRailConsolidationList", "rai-res-consolidationList",
                "Consolidations:List:Rail Consolidation:TranshipmentRailConsolidationList", "rai-tra-consolidationList",
                "Consolidations:List:Rail Consolidation:EmptyContainerRailConsolidationList", "rai-emt-consolidationList",
                "Consolidations:List:Rail Consolidation:DomesticRailConsolidationList", "rai-dom-consolidationList",
                "Consolidations:List:Rail Consolidation:CrossTradeRailConsolidationList", "rai-cts-consolidationList",
                "Consolidations:List:Rail Consolidation:AllRailConsolidationList", "rai-consolidationList",
                "Consolidations:List:Road Consolidation:ImportRoadConsolidationList", "roa-imp-consolidationList",
                "Consolidations:List:Road Consolidation:ExportRoadConsolidationList", "roa-exp-consolidationList",
                "Consolidations:List:Road Consolidation:ReshipmentRoadConsolidationList", "roa-res-consolidationList",
                "Consolidations:List:Road Consolidation:TranshipmentRoadConsolidationList", "roa-tra-consolidationList",
                "Consolidations:List:Road Consolidation:EmptyContainerRoadConsolidationList", "roa-emt-consolidationList",
                "Consolidations:List:Road Consolidation:DomesticRoadConsolidationList", "roa-dom-consolidationList",
                "Consolidations:List:Air Consolidation:ImportAirConsolidationList",
                "Consolidations:List:Air Consolidation:ExportAirConsolidationList",
                "Consolidations:List:Air Consolidation:ReshipmentAirConsolidationList",
                "Consolidations:List:Air Consolidation:TranshipmentAirConsolidationList",
                "Consolidations:List:Air Consolidation:EmptyContainerAirConsolidationList",
                "Consolidations:List:Air Consolidation:DomesticAirConsolidationList",
                "Consolidations:List:Air Consolidation:CrossTradeAirConsolidationList",
                "Consolidations:List:Air Consolidation:AllAirConsolidationList",
                "Consolidations:List:All Consolidation:ImportAllConsolidationList",
                "Consolidations:List:All Consolidation:ExportAllConsolidationList",
                "Consolidations:List:All Consolidation:ReshipmentAllConsolidationList",
                "Consolidations:List:All Consolidation:TranshipmentAllConsolidationList",
                "Consolidations:List:All Consolidation:EmptyContainerAllConsolidationList",
                "Consolidations:List:All Consolidation:DomesticAllConsolidationList",
                "Consolidations:List:All Consolidation:CrossTradeAllConsolidationList",
                "Shipments:Update:Sea International Shipment:ImportSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:ExportSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:ReshipmentSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:TranshipmentSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:EmptyContainerSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:DomesticSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:CrossTradeSeaShipmentUpdate",
                "Shipments:Update:Sea International Shipment:AllSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:ImportDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:ExportDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:ReshipmentDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:TranshipmentDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:EmptyContainerDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:DomesticDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:CrossTradeDomesticSeaShipmentUpdate",
                "Shipments:Update:Sea Domestic Shipment:AllDomesticSeaShipmentUpdate",
                "Shipments:Update:Air Shipment:ImportAirShipmentUpdate",
                "Shipments:Update:Air Shipment:ExportAirShipmentUpdate",
                "Shipments:Update:Air Shipment:ReshipmentAirShipmentUpdate",
                "Shipments:Update:Air Shipment:TranshipmentAirShipmentUpdate",
                "Shipments:Update:Air Shipment:EmptyContainerAirShipmentUpdate",
                "Shipments:Update:Air Shipment:DomesticAirShipmentUpdate",
                "Shipments:Update:Air Shipment:CrossTradeAirShipmentUpdate",
                "Shipments:Update:Air Shipment:AllAirShipmentUpdate",
                "Shipments:Update:All Shipment:ImportAllShipmentUpdate",
                "Shipments:Update:All Shipment:ExportAllShipmentUpdate",
                "Shipments:Update:All Shipment:ReshipmentAllShipmentUpdate",
                "Shipments:Update:All Shipment:TranshipmentAllShipmentUpdate",
                "Shipments:Update:All Shipment:EmptyContainerAllShipmentUpdate",
                "Shipments:Update:All Shipment:DomesticAllShipmentUpdate",
                "Shipments:Update:All Shipment:CrossTradeAllShipmentUpdate",
                "Shipments:Update:All Shipment:AllShipmentUpdate",
                "Shipments:Update:Rail Shipment:ImportRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:ExportRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:ReshipmentRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:TranshipmentRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:EmptyContainerRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:DomesticRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:CrossTradeRailShipmentUpdate",
                "Shipments:Update:Rail Shipment:AllRailShipmentUpdate",
                "Shipments:Update:Road Shipment:ImportRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:ExportRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:ReshipmentRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:TranshipmentRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:EmptyContainerRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:DomesticRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:CrossTradeRoadShipmentUpdate",
                "Shipments:Update:Road Shipment:AllRoadShipmentUpdate",
                "Shipments:Creation:Air Shipment:ImportAirShipmentCreate",
                "Shipments:Creation:Air Shipment:ExportAirShipmentCreate",
                "Shipments:Creation:Air Shipment:ReshipmentAirShipmentCreate",
                "Shipments:Creation:Air Shipment:TranshipmentAirShipmentCreate",
                "Shipments:Creation:Air Shipment:EmptyContainerAirShipmentCreate",
                "Shipments:Creation:Air Shipment:DomesticAirShipmentCreate",
                "Shipments:Creation:Air Shipment:CrossTradeAirShipmentCreate",
                "Shipments:Creation:Air Shipment:AllAirShipmentCreate",
                "Shipments:Creation:All Shipment:ImportAllShipmentCreate",
                "Shipments:Creation:All Shipment:ExportAllShipmentCreate",
                "Shipments:Creation:All Shipment:ReshipmentAllShipmentCreate",
                "Shipments:Creation:All Shipment:TranshipmentAllShipmentCreate",
                "Shipments:Creation:All Shipment:EmptyContainerAllShipmentCreate",
                "Shipments:Creation:All Shipment:DomesticAllShipmentCreate",
                "Shipments:Creation:All Shipment:CrossTradeAllShipmentCreate",
                "Shipments:Creation:All Shipment:AllShipmentCreate",
                "Shipments:Creation:Rail Shipment:ImportRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:ExportRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:ReshipmentRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:TranshipmentRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:EmptyContainerRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:DomesticRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:CrossTradeRailShipmentCreate",
                "Shipments:Creation:Rail Shipment:AllRailShipmentCreate",
                "Shipments:Creation:Road Shipment:ImportRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:ExportRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:ReshipmentRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:TranshipmentRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:EmptyContainerRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:DomesticRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:CrossTradeRoadShipmentCreate",
                "Shipments:Creation:Road Shipment:AllRoadShipmentCreate",
                "Shipments:Creation:Sea International Shipment:ImportSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:ExportSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:ReshipmentSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:TranshipmentSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:EmptyContainerSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:DomesticSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:CrossTradeSeaShipmentCreate",
                "Shipments:Creation:Sea International Shipment:AllSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:ImportDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:ExportDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:ReshipmentDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:TranshipmentDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:EmptyContainerDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:DomesticDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:CrossTradeDomesticSeaShipmentCreate",
                "Shipments:Creation:Sea Domestic Shipment:AllDomesticSeaShipmentCreate"
        ));

        PermissionsContext.setPermissions(permissions);

        List<String> permissionList = PermissionsContext.getPermissions(SHIPMENT_LIST_PERMISSION);
        List<FilterCriteria> result = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(SHIPMENT_RETRIEVE_PERMISSION);
        List<FilterCriteria> result2 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(SHIPMENT_CREATE_PERMISSION);
        List<FilterCriteria> result3 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(SHIPMENT_UPDATE_PERMISSION);
        List<FilterCriteria> result4 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(CONSOLIDATION_LIST_PERMISSION);
        List<FilterCriteria> result5 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(CONSOLIDATION_RETRIEVE_PERMISSION);
        List<FilterCriteria> result6 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(CONSOLIDATION_CREATE_PERMISSION);
        List<FilterCriteria> result7 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(CONSOLIDATION_UPDATE_PERMISSION);
        List<FilterCriteria> result8 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(CARRIER_BOOKING_CREATE);
        List<FilterCriteria> result9 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);


        permissionList = PermissionsContext.getPermissions(CARRIER_BOOKING_VIEW);
        List<FilterCriteria> result10 = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);
        PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);
        PermissionUtil.generateFilterCriteriaFromPermissions(List.of("test"), false);



        assertEquals(15, result.size());
        assertEquals(48, result2.size());
        assertEquals(48, result3.size());
        assertEquals(48, result4.size());
        assertEquals(2, result5.size());
        assertEquals(40, result6.size());
        assertEquals(40, result7.size());
        assertEquals(40, result8.size());
        assertEquals(0, result9.size());
        assertEquals(0, result10.size());


    }

}
