package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGES_SMALL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMPANY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PERSON;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PHONE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOM_HOUSE_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EMAIL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_VOLUME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.INVNO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NET_WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PRE_CARRIAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SEA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TARE_WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VGM_WEIGHT;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.SeawayBillModel;
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
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SeawayBillReportTest extends CommonMocks {

    @InjectMocks
    private SeawayBillReport seawayBillReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private ShipmentService shipmentService;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private HblReport hblReport;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    MasterDataUtils masterDataUtils;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IHblDao hblDao;

    Map<String, TenantModel> mockedTenantMap = new HashMap<>();

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
                V1TenantSettingsResponse.builder().isModuleValidationEnabled(Boolean.TRUE).P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd")
                        .GSTTaxAutoCalculation(true).build());

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
    }

    private void populateModel(SeawayBillModel seawayBillModel) {
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
        orgData.put(ADDRESS1, "123");
        orgData.put(ADDRESS2, "123");
        orgData.put(CITY, "123");
        orgData.put(EMAIL, "123");
        orgData.put(CONTACT_PHONE, "123");
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
        containerModelList.add(containers);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);

        ShipmentOrderModel shipmentOrderModel = new ShipmentOrderModel();
        shipmentOrderModel.setOrderNumber("1234-5678-9123-4567");

        ShipmentOrderModel shipmentOrderModel2 = new ShipmentOrderModel();
        shipmentOrderModel2.setOrderNumber("1235-5678-9123-4567");

        ShipmentOrderModel shipmentOrderModel3 = new ShipmentOrderModel();
        shipmentOrderModel3.setOrderNumber("1235-5679-9123-4567");

        shipmentModel.setShipmentOrders(Arrays.asList(shipmentOrderModel, shipmentOrderModel2, shipmentOrderModel3));

        seawayBillModel.setShipment(shipmentModel);

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

        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(INVNO);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        ArrivalDepartureDetailsModel arrivalDepartureDetailsModel = new ArrivalDepartureDetailsModel();
        arrivalDepartureDetailsModel.setCTOId(partiesModel);
        consolidationModel.setArrivalDetails(arrivalDepartureDetailsModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        seawayBillModel.setConsolidation(consolidationModel);
    }

    private Hbl populateHbl() {
        Hbl hbl = new Hbl();
        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setCargoGrossVolumeUnit("M3");
        hblDataDto.setCargoGrossWeightUnit("KG");
        hblDataDto.setPackageCount(10);
        hbl.setHblData(hblDataDto);
        return hbl;
    }

    @Test
    void testValidatePrinting_ModuleValidationDisabled() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.FALSE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        assertDoesNotThrow(() -> seawayBillReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentNull() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());

        assertThrows(ReportException.class, () -> seawayBillReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentValidationWithSeaTransportAndMissingFields() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doAnswer(invocation -> {
            List<ModuleValidationFieldType> missingFields = invocation.getArgument(1);
            missingFields.add(ModuleValidationFieldType.CARRIER);
            return null;
        }).when(shipmentService).validateCarrierDetails(any(), anyList());

        assertThrows(ReportException.class, () -> seawayBillReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentValidationWithSeaTransportAndMissingFields2() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doAnswer(invocation -> {
            List<ModuleValidationFieldType> missingFields = invocation.getArgument(1);
            missingFields.add(ModuleValidationFieldType.CARRIER);
            return null;
        }).when(shipmentService).validateCarrierDetails(any(), anyList());

        assertThrows(ReportException.class, () -> seawayBillReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentValidationWithSeaTransportAndNoMissingFields() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentService).validateCarrierDetails(any(), anyList());
        doNothing().when(shipmentService).validateContainerDetails(any(), anyList());

        assertDoesNotThrow(() -> seawayBillReport.validatePrinting(123L));
    }

    @Test
    void testValidatePrinting_ShipmentValidationWithDifferentJobType() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT); // Different job type

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        assertDoesNotThrow(() -> seawayBillReport.validatePrinting(123L));
        verify(shipmentService, never()).validateCarrierDetails(any(), anyList());
        verify(shipmentService, never()).validateContainerDetails(any(), anyList());
    }


    @Test
    void populateDictionary() {
        SeawayBillModel seawayBillModel = SeawayBillModel.builder().build();
        seawayBillModel.setBlObject(populateHbl());
        seawayBillModel.setTenant(new TenantModel());
        seawayBillModel.setShipmentSettingsDetails(ShipmentSettingsDetails.builder().disableBlPartiesName(false).build());
        populateModel(seawayBillModel);

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.NOOF_PACKAGES, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.GROSS_VOLUME_ALIAS, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.BL_GROSS_VOLUME_ALIAS, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.BL_GROSS_WEIGHT_ALIAS, BigDecimal.TEN);

        doReturn(Arrays.asList(containerMap)).when(jsonHelper).convertValue(eq(seawayBillModel.shipment.getShipmentContainersList()), any(TypeReference.class));

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        Map<String, Object> dictionary = new HashMap<>();
        Map<String, Object> chargeMap = new HashMap<>();
        chargeMap.put(CHARGE_TYPE_CODE, "AgentCharge");
        dictionary.put(CHARGES_SMALL, Arrays.asList(chargeMap));
        dictionary.put(ReportConstants.ORDER_MANAGEMENT_NUMBER, "1234-5678-9123-4567,1235-5678-9123-4567,1235-5679-9123-4567");
        when(hblReport.getData(any())).thenReturn(dictionary);
        mockTenantSettings();
        Map<String, Object> dict = seawayBillReport.populateDictionary(seawayBillModel);
        assertNotNull(dict);
        assertNotNull(dict.get(ReportConstants.ORDER_MANAGEMENT_NUMBER));
    }

    @Test
    void populateDictionaryWithDisbalePartyTrue() {
        SeawayBillModel seawayBillModel = SeawayBillModel.builder().build();
        seawayBillModel.setBlObject(populateHbl());
        seawayBillModel.setTenant(new TenantModel());
        seawayBillModel.setShipmentSettingsDetails(ShipmentSettingsDetails.builder().disableBlPartiesName(true).build());
        populateModel(seawayBillModel);

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.NOOF_PACKAGES, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.GROSS_VOLUME_ALIAS, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.BL_GROSS_VOLUME_ALIAS, BigDecimal.TEN);
        containerMap.put(SeawayBillReport.BL_GROSS_WEIGHT_ALIAS, BigDecimal.TEN);

        doReturn(Arrays.asList(containerMap)).when(jsonHelper).convertValue(eq(seawayBillModel.shipment.getShipmentContainersList()), any(TypeReference.class));

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        Map<String, Object> dictionary = new HashMap<>();
        Map<String, Object> chargeMap = new HashMap<>();
        chargeMap.put(CHARGE_TYPE_CODE, "AgentCharge");
        dictionary.put(CHARGES_SMALL, Arrays.asList(chargeMap));
        when(hblReport.getData(any())).thenReturn(dictionary);
        mockTenantSettings();
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);
        assertNotNull(seawayBillReport.populateDictionary(seawayBillModel));
    }

    @Test
    void getDocumentModel() {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        shipmentModel.setPickupDetails(new PickupDeliveryDetailsModel());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        HblModel hblModel = new HblModel();
        hblModel.setShipment(shipmentModel);
        when(hblDao.findByShipmentId(any())).thenReturn(new ArrayList<>());
        mockShipmentSettings();
        assertNotNull(seawayBillReport.getDocumentModel(123L));
    }
}
