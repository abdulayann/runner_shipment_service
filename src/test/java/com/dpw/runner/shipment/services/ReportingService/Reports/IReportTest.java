package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.InstructionType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.impl.ShipmentServiceImplV3;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

// A concrete implementation of the abstract IReport class, created solely for testing purposes.

class TestReport extends IReport {
    @Override
    public Map<String, Object> getData(Long id) throws RunnerException { return null; }
    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException { return null; }
    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) throws RunnerException { return null; }
}

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class IReportTest extends CommonMocks {

    @InjectMocks
    @Spy
    private TestReport iReport;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private ShipmentServiceImplV3 shipmentServiceImplV3;

    @Mock
    private MasterDataUtils masterDataUtils;

    private Parties createMockParty(String orgCode, String addressCode, String type, String name) {
        Parties party = new Parties();
        party.setOrgCode(orgCode);
        party.setAddressCode(addressCode);
        party.setType(type);
        party.setOrgData(Map.of(PartiesConstants.FULLNAME, name));
        party.setAddressData(Map.of(
                PartiesConstants.ADDRESS1, "123 Main St",
                PartiesConstants.CITY, "Anytown"
        ));
        return party;
    }

    private TiLegs createMockLeg(long sequence, Parties origin, Parties destination) {
        TiLegs leg = new TiLegs();
        leg.setSequence(sequence);
        leg.setOrigin(origin);
        leg.setDestination(destination);
        return leg;
    }

    private PickupDeliveryDetails createMockTransportInstruction(InstructionType type, Parties transporter, List<TiLegs> legs, List<Parties> additionalParties) {
        PickupDeliveryDetails ti = new PickupDeliveryDetails();
        ti.setType(type);
        ti.setTransporterDetail(transporter);
        ti.setTiLegsList(legs);
        ti.setPartiesList(additionalParties);
        return ti;
    }

@Test
void testPopulateConsolidationReportData_withFirmsCode() {
    // Arrange
    Map<String, Object> dict = new HashMap<>();
    Parties sendingAgent = createMockParty("SEND_AGENT_ORG", "ADDR1", "SendingAgent", "Sending Agent Inc.");
    sendingAgent.setOrgId("SEND_AGENT_ORG");
    Parties receivingAgent = createMockParty("RECV_AGENT_ORG", "ADDR2", "ReceivingAgent", "Receiving Agent Co.");
    receivingAgent.setOrgId("RECV_AGENT_ORG");
    Parties shipper = createMockParty("SHIPPER_ORG", "ADDR3", "Shipper", "Global Shipper");
    shipper.setOrgId("SHIPPER_ORG");
    Parties consigneeNoFirms = createMockParty("NO_FIRMS_ORG", "ADDR4", "Consignee", "No Firms Consignee");
    consigneeNoFirms.setOrgId("NO_FIRMS_ORG");

    ConsolidationDetails consolidation = new ConsolidationDetails();
    consolidation.setSendingAgent(sendingAgent);
    consolidation.setReceivingAgent(receivingAgent);
    consolidation.setConsolidationAddresses(List.of(shipper, consigneeNoFirms));

    Map<String, String> firmsCodeMap = Map.of(
            "SEND_AGENT_ORG", "FIRMS_SEND",
            "RECV_AGENT_ORG", "FIRMS_RECV",
            "SHIPPER_ORG", "FIRMS_SHIP"
    );

    when(consolidationDetailsDao.findConsolidationsById(1L)).thenReturn(consolidation);
    when(mdmServiceAdapter.getFirmsCodeListFromCache(any(Set.class))).thenReturn(firmsCodeMap);

    // Act
    iReport.populateConsolidationReportData(dict, null, 1L);

    // Assert
    List<String> sendingAgentAddress = (List<String>) dict.get("C_SendingAgent");
    assertNotNull(sendingAgentAddress);
    assertTrue(sendingAgentAddress.stream().anyMatch(line -> line.contains("FIRMS_SEND")));

    List<String> receivingAgentAddress = (List<String>) dict.get("C_ReceivingAgent");
    assertNotNull(receivingAgentAddress);
    assertTrue(receivingAgentAddress.stream().anyMatch(line -> line.contains("FIRMS_RECV")));

    List<String> shipperAddress = (List<String>) dict.get("C_Shipper");
    assertNotNull(shipperAddress);
    assertTrue(shipperAddress.stream().anyMatch(line -> line.contains("FIRMS_SHIP")));
    assertTrue(shipperAddress.stream().noneMatch(line -> line.contains("NO_FIRMS_ORG")));
}

    @Test
    void testPopulateShipmentReportData_withTransportInstructionFirmsCode() {
        // Arrange
        Map<String, Object> dict = new HashMap<>();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setAdditionalDetails(new AdditionalDetails());

        // Transport Instruction Parties
        Parties pickupTransporter = createMockParty("PICK_TRANS_ORG", "ADDR_PT", "TransporterDetail", "Pickup Transport");
        pickupTransporter.setOrgId("PICK_TRANS_ORG");
        Parties pickupLeg1Origin = createMockParty("PICK_L1O_ORG", "ADDR_PL1O", "Origin", "Pickup Origin 1");
        pickupLeg1Origin.setOrgId("PICK_L1O_ORG");
        Parties pickupLeg1Dest = createMockParty("PICK_L1D_ORG", "ADDR_PL1D", "Destination", "Pickup Dest 1");
        pickupLeg1Dest.setOrgId("PICK_L1D_ORG");
        Parties pickupExportAgent = createMockParty("PICK_EXA_ORG", "ADDR_PEXA", "ExportAgent", "Pickup Export Agent");
        pickupExportAgent.setOrgId("PICK_EXA_ORG");

        Parties deliveryTransporter = createMockParty("DLV_TRANS_ORG", "ADDR_DT", "TransporterDetail", "Delivery Transport");
        deliveryTransporter.setOrgId("DLV_TRANS_ORG");
        Parties deliveryLeg1Origin = createMockParty("DLV_L1O_ORG", "ADDR_DL1O", "Origin", "Delivery Origin 1");
        deliveryLeg1Origin.setOrgId("DLV_L1O_ORG");

        // Create Transport Instructions
        TiLegs pickupLeg = createMockLeg(1L, pickupLeg1Origin, pickupLeg1Dest);
        PickupDeliveryDetails pickupInstruction = createMockTransportInstruction(InstructionType.Pickup, pickupTransporter, List.of(pickupLeg), List.of(pickupExportAgent));

        TiLegs deliveryLeg = createMockLeg(1L, deliveryLeg1Origin, null); // Destination can be null
        PickupDeliveryDetails deliveryInstruction = createMockTransportInstruction(InstructionType.Delivery, deliveryTransporter, List.of(deliveryLeg), Collections.emptyList());

        List<PickupDeliveryDetails> transportInstructions = List.of(pickupInstruction, deliveryInstruction);

        Map<String, String> firmsCodeMap = Map.of(
                "PICK_TRANS_ORG", "FIRMS_PICK_TRANS",
                "PICK_L1O_ORG", "FIRMS_PICK_L1O",
                "PICK_L1D_ORG", "FIRMS_PICK_L1D",
                "PICK_EXA_ORG", "FIRMS_PICK_EXA",
                "DLV_TRANS_ORG", "FIRMS_DLV_TRANS",
                "DLV_L1O_ORG", "FIRMS_DLV_L1O"
        );

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipment));
        when(pickupDeliveryDetailsDao.findByShipmentId(1L)).thenReturn(transportInstructions);
        when(mdmServiceAdapter.getFirmsCodeListFromCache(any(Set.class))).thenReturn(firmsCodeMap);
        when(shipmentServiceImplV3.getAllMasterData(any(), any())).thenReturn(new HashMap<>());

        // Act
        iReport.populateShipmentReportData(dict, null, 1L);

        // Assert
        assertEquals("FIRMS_PICK_TRANS", dict.get("TI_Pickup_TransporterDetail.FIRMSCode"));
        assertEquals("FIRMS_PICK_L1O", dict.get("TI_Pickup_Leg_1_Origin.FIRMSCode"));
        assertEquals("FIRMS_PICK_L1D", dict.get("TI_Pickup_Leg_1_Destination.FIRMSCode"));
        assertEquals("FIRMS_PICK_EXA", dict.get("TI_Pickup_ExportAgent.FIRMSCode"));

        assertEquals("FIRMS_DLV_TRANS", dict.get("TI_Delivery_TransporterDetail.FIRMSCode"));
        assertEquals("FIRMS_DLV_L1O", dict.get("TI_Delivery_Leg_1_Origin.FIRMSCode"));
        assertFalse(dict.containsKey("TI_Delivery_Leg_1_Destination.FIRMSCode")); // Destination was null
    }

    @Test
    void testPopulateShipmentReportData_withMissingFirmsCode() {
        // Arrange
        Map<String, Object> dict = new HashMap<>();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setAdditionalDetails(new AdditionalDetails());
        Parties client = createMockParty("CLIENT_ORG", "ADDR_C", "Client", "Test Client");
        client.setOrgId("CLIENT_ORG");
        shipment.setClient(client);

        Parties pickupTransporter = createMockParty("PICK_TRANS_ORG", "ADDR_PT", "Transporter Detail", "Pickup Transport"); // test sanitization
        pickupTransporter.setOrgId("PICK_TRANS_ORG");

        Parties extraParty = createMockParty("EXTRA_ORG_MISS", "ADDR_EX", "Export Agent", "Extra Agent");
        extraParty.setOrgId("EXTRA_ORG_MISS");

        PickupDeliveryDetails pickupInstruction = createMockTransportInstruction(
                InstructionType.Pickup,
                pickupTransporter,
                Collections.emptyList(),
                List.of(extraParty) // missing in map
        );

        // Mock that only the client's FIRMS code is found
        Map<String, String> firmsCodeMap = Map.of("CLIENT_ORG", "FIRMS_CLIENT");

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipment));
        when(pickupDeliveryDetailsDao.findByShipmentId(1L)).thenReturn(List.of(pickupInstruction));
        when(mdmServiceAdapter.getFirmsCodeListFromCache(any(Set.class))).thenReturn(firmsCodeMap);
        when(shipmentServiceImplV3.getAllMasterData(any(), any())).thenReturn(new HashMap<>());

        // Act
        iReport.populateShipmentReportData(dict, null, 1L);

        // Assert
        // Client FIRMS code should be present in the formatted address
        List<String> clientAddress = (List<String>) dict.get("S_Client");
        assertNotNull(clientAddress);
        assertTrue(clientAddress.stream().anyMatch(line -> line.contains("FIRMS_CLIENT")));

        // Transporter FIRMS code key should be absent
        assertFalse(dict.containsKey("TI_Pickup_TransporterDetail.FIRMSCode"));
        // Additional party missing in map should not create a key
        assertFalse(dict.containsKey("TI_Pickup_ExportAgent.FIRMSCode"));
    }

    @Test
    void testPopulateShipmentReportData_handlesNullAndEmptyPartiesGracefully() {
        // Arrange
        Map<String, Object> dict = new HashMap<>();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setAdditionalDetails(new AdditionalDetails());

        // Instruction with null transporter and empty legs/parties
        PickupDeliveryDetails pickupInstruction = createMockTransportInstruction(InstructionType.Pickup, null, new ArrayList<>(), new ArrayList<>());

        // Instruction with legs: one with null dest, another with dest having null orgData
        Parties deliveryLeg1Origin = createMockParty("DLV_L1O_ORG", "ADDR_DL1O", "Origin", "Delivery Origin 1");
        deliveryLeg1Origin.setOrgId("DLV_L1O_ORG");
        TiLegs deliveryLeg = createMockLeg(1L, deliveryLeg1Origin, null);

        Parties destWithNullOrgData = new Parties();
        destWithNullOrgData.setOrgId("DLV_L2D_ORG");
        destWithNullOrgData.setOrgCode("DLV_L2D_ORG");
        destWithNullOrgData.setAddressCode("ADDR_DL2D");
        destWithNullOrgData.setType("Destination");
        destWithNullOrgData.setOrgData(null);
        destWithNullOrgData.setAddressData(Map.of(PartiesConstants.ADDRESS1, "Line1", PartiesConstants.CITY, "City"));

        Parties deliveryLeg2Origin = createMockParty("DLV_L2O_ORG", "ADDR_DL2O", "Origin", "Delivery Origin 2");
        deliveryLeg2Origin.setOrgId("DLV_L2O_ORG");
        TiLegs deliveryLeg2 = createMockLeg(2L, deliveryLeg2Origin, destWithNullOrgData);

        PickupDeliveryDetails deliveryInstruction = createMockTransportInstruction(InstructionType.Delivery, null, List.of(deliveryLeg, deliveryLeg2), Collections.emptyList());

        List<PickupDeliveryDetails> transportInstructions = List.of(pickupInstruction, deliveryInstruction);

        Map<String, String> firmsCodeMap = Map.of(
                "DLV_L1O_ORG", "FIRMS_DLV_L1O",
                "DLV_L2O_ORG", "FIRMS_DLV_L2O",
                "DLV_L2D_ORG", "FIRMS_DLV_L2D"
        );

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipment));
        when(pickupDeliveryDetailsDao.findByShipmentId(1L)).thenReturn(transportInstructions);
        when(mdmServiceAdapter.getFirmsCodeListFromCache(any(Set.class))).thenReturn(firmsCodeMap);
        when(shipmentServiceImplV3.getAllMasterData(any(), any())).thenReturn(new HashMap<>());

        // Act
        iReport.populateShipmentReportData(dict, null, 1L);

        // Assert - The method should run without NullPointerExceptions
        assertFalse(dict.containsKey("TI_Pickup_TransporterDetail.FIRMSCode"));
        assertEquals("FIRMS_DLV_L1O", dict.get("TI_Delivery_Leg_1_Origin.FIRMSCode"));
        assertFalse(dict.containsKey("TI_Delivery_Leg_1_Destination.FIRMSCode"));
        assertEquals("FIRMS_DLV_L2O", dict.get("TI_Delivery_Leg_2_Origin.FIRMSCode"));
        assertTrue(dict.containsKey("TI_Delivery_Leg_2_Destination.FIRMSCode"));
        assertEquals("FIRMS_DLV_L2D", dict.get("TI_Delivery_Leg_2_Destination.FIRMSCode"));
    }

}
