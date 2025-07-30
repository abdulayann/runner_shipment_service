package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CSDModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.AdditionalDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ArrivalDepartureDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.CarrierDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.RoutingsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.TruckDriverDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.impl.ShipmentServiceImplV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CSDReportTest {

    @InjectMocks
    CSDReport csdReport;

    @Mock
    MasterDataUtils masterDataUtils;

    @Mock
    CommonUtils commonUtils;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    IAwbDao awbDao;

    @Mock
    IShipmentDao shipmentDao;

    @Mock
    private IContainerDao containerDao;

    Map<String, TenantModel> mockedTenantMap = new HashMap<>();

    @Mock
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    Map<String, Object> mapMock = new HashMap<>();

    @BeforeAll
    static void init() throws IOException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).build());
    }

    @BeforeEach
    void setup()  {
        Map<String, String> nestedStringMap = new HashMap<>();
        nestedStringMap.put("ijk", "lmn");
        Map<String, Object> nestedMap = new HashMap<>();
        nestedMap.put("ORDER_DPW", nestedStringMap);
        mapMock.put("MasterLists", nestedMap);
        mapMock.put("Organizations", nestedStringMap);
    }

    private ShipmentDetails getSampleShipmentDetails() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setControlled(true);
        shipmentDetails.setControlledReferenceNumber("abc");
        shipmentDetails.setIncotermsLocation("INC");
        shipmentDetails.setPartner("Partner");
        shipmentDetails.setCoLoadCarrierName("Co_load_carrier");
        shipmentDetails.setCoLoadBkgNumber("Co_load_bkg_number");
        shipmentDetails.setCoLoadBlNumber("Co_load_bl_number");
        shipmentDetails.setBookingAgent(123L);
        shipmentDetails.setPickupAtOrigin(11L);
        shipmentDetails.setDeliveryAtDestination(12L);
        shipmentDetails.setBrokerageAtOrigin(13L);
        shipmentDetails.setBrokerageAtDestination(14L);
        shipmentDetails.setTerminalCutoff(LocalDateTime.now());
        shipmentDetails.setVerifiedGrossMassCutoff(LocalDateTime.now());
        shipmentDetails.setShippingInstructionCutoff(LocalDateTime.now());
        shipmentDetails.setEarliestEmptyEquipmentPickUp(LocalDateTime.now());
        shipmentDetails.setLatestFullEquipmentDeliveredToCarrier(LocalDateTime.now());
        shipmentDetails.setEarliestDropOffFullEquipmentToCarrier(LocalDateTime.now());
        shipmentDetails.setIsReefer(true);
        shipmentDetails.setLatestArrivalTime(LocalDateTime.now());
        ReferenceNumbers ref1 = new ReferenceNumbers();
        ref1.setType("MAWB");
        ref1.setReferenceNumber("123-456789");

        ReferenceNumbers ref2 = new ReferenceNumbers();
        ref2.setType("HAWB");
        ref2.setReferenceNumber("789-123456");

        shipmentDetails.setReferenceNumbersList(List.of(ref1, ref2));

        Routings first = new Routings();
        first.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        first.setVesselName("First Vessel");
        first.setVoyage("FV001");
        first.setCarrier("Carrier1");
        first.setFlightNumber("FL123");

        Routings last = new Routings();
        last.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        last.setVesselName("Last Vessel");
        last.setVoyage("LV001");
        last.setCarrier("Carrier2");
        last.setFlightNumber("FL999");

        shipmentDetails.setRoutingsList(List.of(first, last));

        Parties shipper = new Parties();
        shipper.setType("Shipper");
        Map<String, Object> orgData = Map.of(PartiesConstants.FULLNAME, "Shipper Ltd.");
        shipper.setOrgData(orgData);
        Map<String, Object> addrData = Map.of(
                PartiesConstants.ADDRESS1, "123 Street",
                PartiesConstants.CITY, "Cityville",
                PartiesConstants.COUNTRY, "India"
        );
        shipper.setAddressData(addrData);

        shipmentDetails.setShipmentAddresses(List.of(shipper));

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setBOENumber("BOE");
        additionalDetails.setBOEDate(LocalDateTime.now());
        additionalDetails.setOwnership(Ownership.Self);
        additionalDetails.setOwnershipName("Owner");
        additionalDetails.setPassedBy(Ownership.Self);
        additionalDetails.setPassedByPerson("Passed");
        shipmentDetails.setAdditionalDetails(additionalDetails);

        shipmentDetails.getAdditionalDetails().setSendingAgent(shipper);
        shipmentDetails.setOriginBranch(100L);
        shipmentDetails.setReceivingBranch(200L);

        TriangulationPartner triangulationPartner = new TriangulationPartner();
        triangulationPartner.setTriangulationPartner(300L);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));

        // Mock tenant models with lowercase values
        TenantModel origin = new TenantModel();
        origin.setDisplayName("origin branch");
        origin.setAddress1("origin addr1");
        origin.setAddress2("origin addr2");
        origin.setCity("origin city");
        origin.setState("origin state");
        origin.setZipPostCode("12345");
        origin.setCountry("origin country");

        TenantModel dest = new TenantModel();
        dest.setDisplayName("dest branch");

        TenantModel triang = new TenantModel();
        triang.setDisplayName("triang branch");

        // Prepare mocked tenant map
        mockedTenantMap.put("100", origin);
        mockedTenantMap.put("200", dest);
        mockedTenantMap.put("300", triang);

        return shipmentDetails;
    }

    private CSDModel getSampleCSDModel(){
        CSDModel csdModel = new CSDModel();
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(123L);
        shipmentModel.setMasterBill("12");
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("INR");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("INR");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setNoOfPacks(10);
        shipmentModel.setSecurityStatus("Test");
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setPacksUnit("PKG");
        shipmentModel.setHouseBill("hsnn1234");
        shipmentModel.setInnerPacks(123);

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setOrgCode("Test");
        partiesModel.setAddressCode("Test");
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
        orgData.put(PartiesConstants.RAW_DATA, "Text");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);

        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setDestinationPort("bb69aefb-0294-4be9-baec-835a431123df");
        carrierDetailModel.setDestination(carrierDetailModel.getDestinationPort());
        carrierDetailModel.setShippingLine("MAERSK");

        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        additionalDetailModel.setExportBroker(partiesModel);
        additionalDetailModel.setImportBroker(partiesModel);
        additionalDetailModel.setScreeningStatus(Arrays.asList(Constants.AOM));
        additionalDetailModel.setExemptionCodes("Test");
        additionalDetailModel.setAomFreeText("Test");
        additionalDetailModel.setGoodsCO("IND");
        additionalDetailModel.setBLChargesDisplay("PPD");
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        shipmentModel.setServiceType("Test");

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        List<ContainerModel> containerModelList = new ArrayList<>();
        ContainerModel containers = new ContainerModel();
        containers.setContainerCount(1L);
        containers.setContainerCode("20GP");
        containers.setNetWeight(BigDecimal.TEN);
        containers.setContainerNumber("CONT000283");
        containers.setGrossVolume(BigDecimal.TEN);
        containers.setGrossVolumeUnit("M3");
        containers.setGrossWeight(BigDecimal.TEN);
        containers.setGrossWeightUnit("KG");
        containers.setPacksType("PKG");
        containers.setPacks("100");
        containers.setCarrierSealNumber("Test123");
        containerModelList.add(containers);

        containers = new ContainerModel();
        containers.setContainerCount(1L);
        containers.setContainerCode("20GP");
        containers.setNetWeight(BigDecimal.TEN);
        containers.setContainerNumber("CONT000283");
        containers.setGrossVolume(BigDecimal.TEN);
        containers.setGrossVolumeUnit("M3");
        containers.setGrossWeight(BigDecimal.TEN);
        containers.setGrossWeightUnit("KG");
        containers.setPacksType("PKG");
        containers.setPacks("100");
        containers.setCarrierSealNumber("Test123");
        containerModelList.add(containers);
        shipmentModel.setContainersList(containerModelList);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        List<PackingModel> packingModels = new ArrayList<>();
        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setPacks("10");
        packingModels.add(packingModel);

        PackingModel packingModel2 = new PackingModel();
        packingModel2.setLength(BigDecimal.TEN);
        packingModel2.setWidth(BigDecimal.TEN);
        packingModel2.setHeight(BigDecimal.TEN);
        packingModel2.setPacks("20");
        packingModels.add(packingModel2);
        shipmentModel.setPackingList(packingModels);

        List<TruckDriverDetailsModel> truckDriverDetailsModels = new ArrayList<>();
        TruckDriverDetailsModel truckDriverDetailsModel = new TruckDriverDetailsModel();
        truckDriverDetailsModel.setTransporterType(Ownership.Self);
        truckDriverDetailsModels.add(truckDriverDetailsModel);
        shipmentModel.setTruckDriverDetails(truckDriverDetailsModels);


        List<ReferenceNumbersModel> referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReferenceNumbersConstants.REF_NUM_TYPE_ETN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReferenceNumbersConstants.REF_NUM_TYPE_CRR);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        consolidationModel.setPackingList(packingModels);
        ArrivalDepartureDetailsModel arrivalDepartureDetailsModel = new ArrivalDepartureDetailsModel();
        arrivalDepartureDetailsModel.setCTOId(partiesModel);
        arrivalDepartureDetailsModel.setLastForeignPort(UUID.randomUUID().toString());
        consolidationModel.setArrivalDetails(arrivalDepartureDetailsModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        consolidationModel.setShipmentType(EXP);
        consolidationModel.setDepartureDetails(arrivalDepartureDetailsModel);
        consolidationModel.setContainersList(shipmentModel.getContainersList());
        consolidationModel.setId(123L);
        consolidationModel.setShipmentsList(Arrays.asList(shipmentModel));

        csdModel.setShipmentModel(shipmentModel);
        csdModel.setConsolidationModel(consolidationModel);
        return csdModel;
    }

    @Test
    void test_getData() throws RunnerException {
        var spyReport = Mockito.spy(csdReport);
        doReturn(getSampleCSDModel()).when(spyReport).getDocumentModel(Mockito.any());
        doReturn(Collections.emptyMap()).when(spyReport).populateDictionary(Mockito.any());
        var resp = spyReport.getData(12L);
        assertNotNull(resp);
    }

    @Test
    void test_getDocumentModel() throws RunnerException {
        var spyReport = Mockito.spy(this.csdReport);
        doReturn(new ConsolidationModel()).when(spyReport).getConsolidation(Mockito.any());
        spyReport.setIsConsolidation(true);
        var resp = spyReport.getDocumentModel(12L);
        assertNotNull(resp);
    }

    @Test
    void test_getDocumentModel_shipment() throws RunnerException {
        var spyReport = Mockito.spy(this.csdReport);
        doReturn(new ShipmentModel()).when(spyReport).getShipment(anyLong());
        spyReport.setIsConsolidation(false);
        var resp = spyReport.getDocumentModel(12L);
        assertNotNull(resp);
    }

    @Test
    void test_populateDictionary_whenConsolidation() {
        var spyReport = Mockito.spy(this.csdReport);
        doNothing().when(spyReport).populateUserFields(any(), any());
        spyReport.setIsConsolidation(true);
        doNothing().when(spyReport).populateConsolidationFields(any(), any());
        doNothing().when(spyReport).populateRaKcDataConsolidation(any(), any());
        V1TenantSettingsResponse sampleResponse = new V1TenantSettingsResponse();
        sampleResponse.setDPWDateFormat("yyyy-MM-dd");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(sampleResponse);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        consolidationModel.setCarrierDetails(new CarrierDetailModel());
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        var resp = spyReport.populateDictionary(getSampleCSDModel());
        assertNotNull(resp);
    }

    @Test
    void test_populateDictionary_whenConsolidationWithScreeningStatus() {
        var spyReport = Mockito.spy(this.csdReport);
        doNothing().when(spyReport).populateUserFields(any(), any());
        spyReport.setIsConsolidation(true);
        doNothing().when(spyReport).populateConsolidationFields(any(), any());
        doNothing().when(spyReport).populateRaKcDataConsolidation(any(), any());
        V1TenantSettingsResponse sampleResponse = new V1TenantSettingsResponse();
        sampleResponse.setDPWDateFormat("yyyy-MM-dd");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(sampleResponse);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        consolidationModel.setCarrierDetails(new CarrierDetailModel());
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        CSDModel csdModel = getSampleCSDModel();
        csdModel.getConsolidationModel().setScreeningStatus(List.of("AOM", "SCC"));
        var resp = spyReport.populateDictionary(csdModel);
        assertNotNull(resp);
    }

    @Test
    void test_populateDictionary_whenShipment() {
        var spyReport = Mockito.spy(this.csdReport);
        doNothing().when(spyReport).populateUserFields(any(), any());
        spyReport.setIsConsolidation(false);
        doNothing().when(spyReport).populateShipmentFields(any(), any());
        doNothing().when(spyReport).populateRaKcData(any(), any());
        V1TenantSettingsResponse sampleResponse = new V1TenantSettingsResponse();
        sampleResponse.setDPWDateFormat("yyyy-MM-dd");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(sampleResponse);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(getSampleShipmentDetails()));
        when(shipmentServiceImplV3.getAllMasterData(any(), eq(SHIPMENT))).thenReturn(mapMock);
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);
        var resp = spyReport.populateDictionary(getSampleCSDModel());
        assertNotNull(resp);
    }

    @Test
    void testPopulateDictionaryPutsSPXCodeInExemptionCargo() {
        var spyReport = Mockito.spy(this.csdReport);
        CSDModel csdModel = getSampleCSDModel();
        ShipmentModel shipment = csdModel.getShipmentModel();
        doAnswer(invocationOnMock -> {
            Object[] args = invocationOnMock.getArguments();
            Map<String, Object> dictionary = (Map<String, Object>) args[1];
            dictionary.put(CONSIGNMENT_STATUS, shipment.getSecurityStatus());
            return null;
        }).when(spyReport).populateUserFields(any(), any());
        spyReport.setIsConsolidation(false);
        doNothing().when(spyReport).populateShipmentFields(any(), any());
        doNothing().when(spyReport).populateRaKcData(any(), any());
        V1TenantSettingsResponse sampleResponse = new V1TenantSettingsResponse();
        sampleResponse.setDPWDateFormat("yyyy-MM-dd");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(sampleResponse);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(getSampleShipmentDetails()));
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);
        when(shipmentServiceImplV3.getAllMasterData(any(), eq(SHIPMENT))).thenReturn(mapMock);

        shipment.setSecurityStatus(AwbConstants.EXEMPTION_CARGO_SECURITY_STATUS);
        var resp = spyReport.populateDictionary(csdModel);
        assertNotNull(resp);
        assertEquals(AwbConstants.SPX, resp.get(CONSIGNMENT_STATUS));
    }

    @Test
    void testPopulateDictionaryPopulatesListOfAirports() {
        var spyReport = Mockito.spy(this.csdReport);
        doNothing().when(spyReport).populateUserFields(any(), any());
        spyReport.setIsConsolidation(false);
        doNothing().when(spyReport).populateShipmentFields(any(), any());
        doNothing().when(spyReport).populateRaKcData(any(), any());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(new V1TenantSettingsResponse());
        V1TenantSettingsResponse sampleResponse = new V1TenantSettingsResponse();
        sampleResponse.setDPWDateFormat("yyyy-MM-dd");
        CSDModel csdModel = getSampleCSDModel();
        ShipmentModel shipment = csdModel.getShipmentModel();
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(AIR);
        routingsModel.setPol("Airport1");
        routingsModel.setPod("Airport2");
        routingsModel.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        shipment.setRoutingsList(List.of(routingsModel));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(getSampleShipmentDetails()));
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);
        when(shipmentServiceImplV3.getAllMasterData(any(), eq(SHIPMENT))).thenReturn(mapMock);

        var resp = spyReport.populateDictionary(csdModel);
        assertNotNull(resp);
        assertNotNull(resp.get(ReportConstants.TRANSIT_AIRPORTS));
    }

}