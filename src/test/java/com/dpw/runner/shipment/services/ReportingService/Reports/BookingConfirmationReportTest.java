package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingConfirmationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.impl.ShipmentServiceImplV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class BookingConfirmationReportTest extends CommonMocks {

    @InjectMocks
    private BookingConfirmationReport bookingConfirmationReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private HblReport hblReport;

    @Mock
    IShipmentDao shipmentDao;

    @Mock
    MasterDataUtils masterDataUtils;

    @Mock
    private IContainerDao containerDao;

    Map<String, TenantModel> mockedTenantMap = new HashMap<>();

    @Mock
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    Map<String, Object> mapMock = new HashMap<>();

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).build());
    }


    private static ShipmentDetails shipmentDetails;
    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).build());
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

        Map<String, String> nestedStringMap = new HashMap<>();
        nestedStringMap.put("ijk", "lmn");
        Map<String, Object> nestedMap = new HashMap<>();
        nestedMap.put("ORDER_DPW", nestedStringMap);
        mapMock.put("MasterLists", nestedMap);
        mapMock.put("Organizations", nestedStringMap);
    }

    private void populateModel(BookingConfirmationModel bookingConfirmationModel) {
        HblModel hblModel = new HblModel();
        hblModel.setPolName("Test");
        hblModel.setPolCountry("Test");
        hblModel.setPolPort(new UnlocationsResponse());
        hblModel.setPodCountry("Test");
        hblModel.setPodPort(new UnlocationsResponse());
        hblModel.setPodName("Test");

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(123L);
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
        carrierDetailModel.setDestinationPort(UUID.randomUUID().toString());
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
        hblModel.setShipment(shipmentModel);

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


        List<ReferenceNumbersModel> referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType("BKG");
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReferenceNumbersConstants.FEEDER_VESSEL);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReferenceNumbersConstants.MOTHER_VESSEL);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);
        bookingConfirmationModel.setReferenceNumbersList(referenceNumbersModels);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
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
        bookingConfirmationModel.hblModel = hblModel;
    }

    @Test
    void populateDictionary() {
        BookingConfirmationModel bookingConfirmationModel = new BookingConfirmationModel();
        populateModel(bookingConfirmationModel);
        Map<String, Object> dictionary = new HashMap<>();
        Map<String, Object> chargeMap = new HashMap<>();
        chargeMap.put(CHARGE_TYPE_CODE, "AgentCharge");
        dictionary.put(CHARGES_SMALL, Arrays.asList(chargeMap));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);
        when(hblReport.populateDictionary(any())).thenReturn(dictionary);
        mockTenantSettings();
        when(shipmentServiceImplV3.getAllMasterData(any(), eq(SHIPMENT))).thenReturn(mapMock);
        assertNotNull(bookingConfirmationReport.populateDictionary(bookingConfirmationModel));
    }

    @Test
    void populateDictionaryWithNonBkg() {
        BookingConfirmationModel bookingConfirmationModel = new BookingConfirmationModel();
        populateModel(bookingConfirmationModel);
        List<ReferenceNumbersModel> referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType("NON-BKG");
        referenceNumbersModels.add(referenceNumbersModel);
        bookingConfirmationModel.setReferenceNumbersList(referenceNumbersModels);
        Map<String, Object> dictionary = new HashMap<>();
        Map<String, Object> chargeMap = new HashMap<>();
        chargeMap.put(CHARGE_TYPE_CODE, "AgentCharge");
        dictionary.put(CHARGES_SMALL, Arrays.asList(chargeMap));
        when(hblReport.populateDictionary(any())).thenReturn(dictionary);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentServiceImplV3.getAllMasterData(any(), eq(SHIPMENT))).thenReturn(mapMock);
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);
        mockTenantSettings();
        assertNotNull(bookingConfirmationReport.populateDictionary(bookingConfirmationModel));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        HblModel hblModel = new HblModel();
        hblModel.setShipment(new ShipmentModel());
        when(hblReport.getDocumentModel(any())).thenReturn(hblModel);
        assertNotNull(bookingConfirmationReport.getDocumentModel(123L));
    }
}
