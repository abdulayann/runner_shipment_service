package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbSpecialHandlingCodesMappingInfo;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CargoManifestAirConsolidationReportTest extends CommonMocks {

    @InjectMocks
    private CargoManifestAirConsolidationReport cargoManifestAirConsolidationReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private IAwbDao awbDao;

    @Mock
    private IPackingService packingService;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private Cache cache;

    @Mock
    private CustomKeyGenerator keyGenerator;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).build());
    }


    private static ShipmentDetails shipmentDetails;
    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).build());
    }

    private void mockVessel() {
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());
    }

    private void populateModel(CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel) {
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
        cargoManifestAirConsolidationModel.setShipmentModelList(Arrays.asList(shipmentModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        List<PackingModel> packingModels = new ArrayList<>();
        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setPacks("10");
        packingModel.setCommodity("Bag");
        packingModel.setCommodityGroup("Bag");
        packingModel.setPacksType("Bag");
        packingModel.setWeight(BigDecimal.TEN);
        packingModel.setVolume(BigDecimal.TEN);
        packingModel.setVolumeWeight(BigDecimal.TEN);

        packingModels.add(packingModel);

        PackingModel packingModel2 = new PackingModel();
        packingModel2.setLength(BigDecimal.TEN);
        packingModel2.setWidth(BigDecimal.TEN);
        packingModel2.setHeight(BigDecimal.TEN);
        packingModel2.setPacks("20");
        packingModels.add(packingModel2);
        shipmentModel.setPackingList(packingModels);

        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReportConstants.MoRN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        ArrivalDepartureDetailsModel arrivalDepartureDetailsModel = new ArrivalDepartureDetailsModel();
        arrivalDepartureDetailsModel.setCTOId(partiesModel);
        arrivalDepartureDetailsModel.setLastForeignPort("123");
        consolidationModel.setArrivalDetails(arrivalDepartureDetailsModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        cargoManifestAirConsolidationModel.setConsolidationModel(consolidationModel);
    }

    private Hbl populateHbl(){
        Hbl hbl = new Hbl();
        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setCargoGrossVolumeUnit("M3");
        hblDataDto.setCargoGrossWeightUnit("KG");
        hblDataDto.setPackageCount(10);
        hbl.setHblData(hblDataDto);
        return hbl;
    }

    private void mockRakc(ShipmentModel shipmentModel) {
        Parties parties = new Parties();
        parties.setOrgCode("Test");
        parties.setAddressCode("Test");
        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> addressDataMap = new HashMap<>();
        addressDataMap.put(REGULATED_AGENT, true);
        addressDataMap.put(KCRA_NUMBER, ONE);
        addressDataMap.put(KCRA_EXPIRY, LocalDateTime.now());
        addressMap.put(parties.getOrgCode()+"#"+parties.getAddressCode(), addressDataMap);

        Parties parties2 = new Parties();
        parties2.setOrgCode("Test2");
        parties2.setAddressCode("Test2");
        addressDataMap = new HashMap<>();
        addressDataMap.put(KNOWN_CONSIGNOR, true);
        addressDataMap.put(KCRA_NUMBER, TWO);
        addressDataMap.put(KCRA_EXPIRY, LocalDateTime.now());
        addressMap.put(parties2.getOrgCode()+"#"+parties2.getAddressCode(), addressDataMap);

        OrgAddressResponse orgAddressResponse = new OrgAddressResponse();
        orgAddressResponse.setAddresses(addressMap);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);
    }

    private void mockRaKc(ConsolidationModel consolidationModel) {
        Parties parties = new Parties();
        parties.setOrgCode("Test");
        parties.setAddressCode("Test");
        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> addressDataMap = new HashMap<>();
        addressDataMap.put(REGULATED_AGENT, true);
        addressDataMap.put(KCRA_NUMBER, ONE);
        addressDataMap.put(KCRA_EXPIRY, LocalDateTime.now());
        addressMap.put(parties.getOrgCode()+"#"+parties.getAddressCode(), addressDataMap);
        OrgAddressResponse orgAddressResponse = new OrgAddressResponse();
        orgAddressResponse.setAddresses(addressMap);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
    }

    @Test
    void populateDictionary() {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = new CargoManifestAirConsolidationModel();
        cargoManifestAirConsolidationModel.setTenantModel(new TenantModel());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(new PackSummaryResponse());

        Awb awb = new Awb();
        awb.setAwbCargoInfo(new AwbCargoInfo());

        AwbSpecialHandlingCodesMappingInfo awbSpecialHandlingCodesMappingInfo = new AwbSpecialHandlingCodesMappingInfo();
        awbSpecialHandlingCodesMappingInfo.setShcId("123");
        awb.setAwbSpecialHandlingCodesMappings(Arrays.asList(awbSpecialHandlingCodesMappingInfo));
        cargoManifestAirConsolidationModel.setAwbList(Arrays.asList(awb));
        populateModel(cargoManifestAirConsolidationModel);
        mockVessel();
        cargoManifestAirConsolidationReport.setSecurityData(false);


        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(cargoManifestAirConsolidationModel.getShipmentModelList().get(0));
        mockUnloc();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(cargoManifestAirConsolidationReport.populateDictionary(cargoManifestAirConsolidationModel));
    }

    @Test
    void populateDictionary2() {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = new CargoManifestAirConsolidationModel();
        cargoManifestAirConsolidationModel.setTenantModel(new TenantModel());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(new PackSummaryResponse());

        Awb awb = new Awb();
        awb.setAwbCargoInfo(new AwbCargoInfo());

        AwbSpecialHandlingCodesMappingInfo awbSpecialHandlingCodesMappingInfo = new AwbSpecialHandlingCodesMappingInfo();
        awbSpecialHandlingCodesMappingInfo.setShcId("123");
        awb.setAwbSpecialHandlingCodesMappings(Arrays.asList(awbSpecialHandlingCodesMappingInfo));
        cargoManifestAirConsolidationModel.setAwbList(Arrays.asList(awb));
        populateModel(cargoManifestAirConsolidationModel);
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel2.setCarrier("test2");
        List<RoutingsModel> routingsModels = new ArrayList<>();
        routingsModels.add(routingsModel);
        routingsModels.add(routingsModel2);
        cargoManifestAirConsolidationModel.getConsolidationModel().setRoutingsList(routingsModels);
        mockVessel();
        cargoManifestAirConsolidationReport.setSecurityData(false);


        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(cargoManifestAirConsolidationModel.getShipmentModelList().get(0));
        mockUnloc();
        mockShipmentSettings();
        mockTenantSettings();when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(cargoManifestAirConsolidationReport.populateDictionary(cargoManifestAirConsolidationModel));
    }

    @Test
    void populateDictionaryWithShipmentTypeImp() {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = new CargoManifestAirConsolidationModel();
        cargoManifestAirConsolidationModel.setTenantModel(new TenantModel());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(new PackSummaryResponse());
        populateModel(cargoManifestAirConsolidationModel);
        ShipmentModel shipmentModel = cargoManifestAirConsolidationModel.getShipmentModelList().get(0);
        shipmentModel.setDirection(IMP);
        mockVessel();
        cargoManifestAirConsolidationReport.setSecurityData(true);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(cargoManifestAirConsolidationModel.getShipmentModelList().get(0));
        mockUnloc();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(cargoManifestAirConsolidationReport.populateDictionary(cargoManifestAirConsolidationModel));
    }

    @Test
    void populateDictionaryWithShipmentNull() {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = new CargoManifestAirConsolidationModel();
        cargoManifestAirConsolidationModel.setTenantModel(new TenantModel());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(new PackSummaryResponse());
        populateModel(cargoManifestAirConsolidationModel);
        cargoManifestAirConsolidationModel.setShipmentModelList(null);
        cargoManifestAirConsolidationReport.setSecurityData(true);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRaKc(cargoManifestAirConsolidationModel.getConsolidationModel());
        mockUnloc();
        mockTenantSettings();
        assertNotNull(cargoManifestAirConsolidationReport.populateDictionary(cargoManifestAirConsolidationModel));
    }

    private void masterDataMock() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<MasterData> masterDataList = new ArrayList<>();
        MasterData masterData = new MasterData();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(masterDataList);
    }

    private void mockCarrier() {
        CarrierMasterData carrierMasterData = new CarrierMasterData();
        carrierMasterData.setIataCode("123");
        carrierMasterData.setItemDescription("123");
        carrierMasterData.setItemValue("Turkish Airlines");
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(carrierMasterData)).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(carrierMasterData));
    }

    private void mockUnloc() {
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(unlocationsResponse)).build();
        when(v1MasterData.fetchUnlocationData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), UnlocationsResponse.class)).thenReturn(Arrays.asList(unlocationsResponse));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(false);
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(1L);
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(any(), eq(ShipmentModel.class))).thenReturn(shipmentModel);


        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        CarrierDetails carrierDetails = new CarrierDetails();
        shipmentDetails.setCarrierDetails(carrierDetails);
        List<ShipmentDetails> shipmentDetails1 = new ArrayList<>();
        shipmentDetails1.add(shipmentDetails);

        shipmentDetails = new ShipmentDetails();
        shipmentDetails.setCarrierDetails(carrierDetails);
        shipmentDetails1.add(shipmentDetails);

        when(shipmentDao.findShipmentsByIds(any())).thenReturn(shipmentDetails1);

        Awb awb = new Awb();
        awb.setShipmentId(1L);
        awb.setAwbCargoInfo(new AwbCargoInfo());

        AwbSpecialHandlingCodesMappingInfo awbSpecialHandlingCodesMappingInfo = new AwbSpecialHandlingCodesMappingInfo();
        awbSpecialHandlingCodesMappingInfo.setShcId("123");
        awb.setAwbSpecialHandlingCodesMappings(Arrays.asList(awbSpecialHandlingCodesMappingInfo));
        when(awbDao.findByShipmentIdList(any())).thenReturn(Arrays.asList(awb));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        consolidationModel.setContainersList(Arrays.asList(new ContainerModel()));
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        cargoManifestAirConsolidationReport.setShipIds(Arrays.asList(1L,2L));
        mockShipmentSettings();
        assertNotNull(cargoManifestAirConsolidationReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModelWithShipmentNull() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(false);
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(1L);
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));

        Awb awb = new Awb();
        awb.setShipmentId(1L);
        awb.setAwbCargoInfo(new AwbCargoInfo());

        AwbSpecialHandlingCodesMappingInfo awbSpecialHandlingCodesMappingInfo = new AwbSpecialHandlingCodesMappingInfo();
        awbSpecialHandlingCodesMappingInfo.setShcId("123");
        awb.setAwbSpecialHandlingCodesMappings(Arrays.asList(awbSpecialHandlingCodesMappingInfo));
        when(awbDao.findByShipmentIdList(any())).thenReturn(Arrays.asList(awb));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        consolidationModel.setContainersList(Arrays.asList(new ContainerModel()));
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        cargoManifestAirConsolidationReport.setShipIds(Arrays.asList(1L,2L));
        cargoManifestAirConsolidationReport.setShipperAndConsignee(true);
        mockShipmentSettings();
        assertNotNull(cargoManifestAirConsolidationReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        consolidationModel.setTransportMode(AIR);
        consolidationModel.setShipmentType(EXP);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        consolidationModel.setContainersList(Arrays.asList(new ContainerModel()));
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);

        cargoManifestAirConsolidationReport.setShipIds(Arrays.asList(1L,2L));
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> cargoManifestAirConsolidationReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        UserContext.getUser().getPermissions().put(PermissionConstants.AIR_SECURITY_PERMISSION, true);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        consolidationModel.setTransportMode(AIR);
        consolidationModel.setShipmentType(EXP);
        consolidationModel.setHazardous(true);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        consolidationModel.setContainersList(Arrays.asList(new ContainerModel()));
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);

        cargoManifestAirConsolidationReport.setShipIds(Arrays.asList(1L,2L));
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> cargoManifestAirConsolidationReport.getDocumentModel(123L));
    }

    @ParameterizedTest
    @ValueSource(strings = {"2 BBG, 1 BAG, 3 AMM", "2 BBG, 4 BBG"})
    void testGetTotalPacksAndUnit(String totalPacks) {
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacks(totalPacks);

        String updatedPacksValue = cargoManifestAirConsolidationReport.getTotalPacksAndUnit(packSummaryResponse);
        assertEquals("6 Pieces", updatedPacksValue);
    }

}
