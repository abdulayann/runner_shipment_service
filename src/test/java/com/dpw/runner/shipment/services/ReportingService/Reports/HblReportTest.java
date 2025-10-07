package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CEN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PERSON;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOM_HOUSE_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ERN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FRN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGINAL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PRE_CARRIAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SEA;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.AdditionalDetailModel;
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
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblFreightsAndCharges;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.impl.HblService;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HblReportTest extends CommonMocks {

    @InjectMocks
    private HblReport hblReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private IV1Service v1Service;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IHblDao hblDao;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private ShipmentService shipmentService;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private Cache cache;

    @Mock
    private CustomKeyGenerator keyGenerator;

    @Mock
    private HblService hblService;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    private static ShipmentDetails shipmentDetails;

    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).WVDigitGrouping(2).WVGroupingNumber(GroupingNumber.DotAndComma.getValue()).build());
    }

    @Test
    void testValidatePrinting() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentService).validateCarrierDetails(any(), anyList());
        doNothing().when(shipmentService).validateContainerDetails(any(), anyList());
        assertDoesNotThrow(() -> hblReport.validatePrinting(123L, ORIGINAL));
    }

    @Test
    void testValidatePrinting_ShipmentNull() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        assertThrows(ReportException.class, () -> hblReport.validatePrinting(123L, ORIGINAL));
    }

    @Test
    void testValidatePrinting_MissingFields() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

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

        assertThrows(ReportException.class, () -> hblReport.validatePrinting(123L, ReportConstants.ORIGINAL));
    }

    @Test
    void testValidatePrinting_LCLShipmentType() {
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setIsModuleValidationEnabled(Boolean.TRUE);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentService).validateCarrierDetails(any(), anyList());
        doNothing().when(shipmentService).validateContainerDetails(any(), anyList());

        assertDoesNotThrow(() -> hblReport.validatePrinting(123L, ReportConstants.ORIGINAL));
    }

    @Test
    void getDocumentModelWithoutBlObject() {
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(1))).thenReturn(Arrays.asList(ShipmentSettingsDetails.builder().build()));
        when(hblDao.findByShipmentId(any())).thenReturn(new ArrayList<>());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        assertNotNull(hblReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModelWithBlObject() {
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));

        List<ContainerModel> containerModelList = new ArrayList<>();
        ContainerModel shipmentContainers = new ContainerModel();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setContainerNumber("CONT000283");
        shipmentContainers.setGrossVolume(BigDecimal.TEN);
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeight(BigDecimal.TEN);
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setPacksType("PKG");
        shipmentContainers.setPacks("100");
        containerModelList.add(shipmentContainers);
        shipmentContainers = new ContainerModel();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setContainerNumber("CONT000284");
        shipmentContainers.setGrossVolume(BigDecimal.TEN);
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeight(BigDecimal.TEN);
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setPacksType("PKG");
        shipmentContainers.setPacks("100");
        containerModelList.add(shipmentContainers);
        shipmentModel.setContainersList(containerModelList);

        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setServiceType("TXT");
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setPacksUnit("PKG");
        shipmentModel.setVolumeUnit("M3");
        shipmentModel.setNetWeightUnit("KG");


        String locationGuid = UUID.randomUUID().toString();
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setReleaseType("ORG");
        additionalDetailModel.setPaidPlace(locationGuid);
        additionalDetailModel.setPlaceOfIssue(locationGuid);
        additionalDetailModel.setPlaceOfSupply(locationGuid);
        shipmentModel.setAdditionalDetails(additionalDetailModel);

        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOriginPort(locationGuid);
        carrierDetailModel.setDestination(locationGuid);
        carrierDetailModel.setDestinationPort(locationGuid);
        carrierDetailModel.setOrigin(locationGuid);
        shipmentModel.setCarrierDetails(carrierDetailModel);

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        bookingCarriageModel.setVessel(locationGuid);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setCountry("IND");
        locationMap.put(locationGuid, unlocationsResponse);
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = new HashMap<>();
        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(entityTransferUnLocationsMap);
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(1))).thenReturn(Arrays.asList(ShipmentSettingsDetails.builder().build()));

        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hbl.setHblData(hblDataDto);

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));

        List<HblContainerDto> hblContainerDtos = new ArrayList<>();
        HblContainerDto hblContainerDto = new HblContainerDto();
        hblContainerDto.setContainerType("20GP");
        hblContainerDto.setContainerNumber("CONT000283");
        hblContainerDtos.add(hblContainerDto);
        hblContainerDto = new HblContainerDto();
        hblContainerDto.setContainerType("20GP");
        hblContainerDto.setContainerNumber("CONT000284");
        hblContainerDto.setContainerGrossWeight(BigDecimal.TEN);
        hblContainerDto.setContainerGrossVolume(BigDecimal.TEN);
        hblContainerDtos.add(hblContainerDto);

        hbl.setHblContainer(hblContainerDtos);

        when(hblDao.findByShipmentId(any())).thenReturn(Arrays.asList(hbl));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(masterDataUtils.fetchMasterListFromCache(any())).thenReturn(new HashMap<>());

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
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(hblReport.getDocumentModel(123L));
    }

    @Test
    void populateDictionary() throws JsonProcessingException {
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);
        hbl.setShipmentId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hbl.setHblData(hblDataDto);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        hblModel.setCommonContainers(Arrays.asList(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .partiesList(List.of(
                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("DAG").orgData(Map.of("FullName", "name", "ContactPhone","88")).addressData(Map.of()).build()
                ))
                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
                .transporterDetail(PartiesModel.builder().type("EXA").build())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
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
        ReferenceNumbersModel ernReferenceNumbersModel = new ReferenceNumbersModel();
        ernReferenceNumbersModel.setType(ERN);
        ReferenceNumbersModel cenReferenceNumbersModel = new ReferenceNumbersModel();
        cenReferenceNumbersModel.setType(CEN);
        ReferenceNumbersModel frnReferenceNumbersModel = new ReferenceNumbersModel();
        frnReferenceNumbersModel.setType(FRN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setWeight(BigDecimal.TEN);
        packingModel.setWeightUnit("KG");
        packingModel.setVolume(BigDecimal.TEN);
        packingModel.setVolumeUnit("M3");
        packingModel.setPacks("5");
        packingModel.setPacksType("BAL");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));
        hblModel.setTransportInstructionId(12L);
        hblModel.setShipment(shipmentModel);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
        additionalDetails.setPlaceOfIssue("mumbai");
        hblModel.shipment.setAdditionalDetails(additionalDetails);
        EntityTransferAddress entityAddress = new EntityTransferAddress();
        entityAddress.setCountry("India");
        when(commonUtils.getEntityTransferAddress(hblModel.shipment.getTransportMode())).thenReturn(entityAddress);
        assertNotNull(hblReport.populateDictionary(hblModel));
    }

    @Test
    void testPopulateFreightsAndCharges_nullAdditionalDetails() {

        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(null);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        Map<String, Object> dict = new HashMap<>();
        hblReport.populateFreightsAndCharges(dict, hbl);
        assertTrue(dict.isEmpty());
    }

    @Test
    void testPopulateFreightsAndCharges_nullShipmentDetails() {

        Hbl hbl = new Hbl();
        hbl.setShipmentId(null);

        Map<String, Object> dict = new HashMap<>();
        hblReport.populateFreightsAndCharges(dict, hbl);
        assertTrue(dict.isEmpty());
    }

    @Test
    void testPopulateFreightsAndCharges_emptyShipmentDetails() {

        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);
        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());

        Map<String, Object> dict = new HashMap<>();
        hblReport.populateFreightsAndCharges(dict, hbl);
        assertTrue(dict.isEmpty());
    }

    @Test
    void testPopulateFreightsAndCharges_nullHbl() {

        Map<String, Object> dict = new HashMap<>();
        hblReport.populateFreightsAndCharges(dict, null);
        assertTrue(dict.isEmpty());
    }

    @Test
    void populateFreightsAndCharges_whenIsRatedBLTrueAndValidCharges() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        HblFreightsAndCharges hblFreightsAndCharges = new HblFreightsAndCharges();
        hblFreightsAndCharges.setCharges("100");
        hblFreightsAndCharges.setChargeType("PREPAID");
        hblFreightsAndCharges.setValue(11.00);
        hblFreightsAndCharges.setCurrency("INR");
        hbl.setHblFreightsAndCharges(List.of(hblFreightsAndCharges));

        Map<String, Object> dictionary = new HashMap<>();
        hblReport.populateFreightsAndCharges(dictionary, hbl);
        assertTrue(dictionary.containsKey("freightsAndCharges")
                || !dictionary.isEmpty(), "Should populate dictionary with charges");
    }

    @Test
    void populateFreightsAndCharges_whenIsRatedBLTrueNullCurrency() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        HblFreightsAndCharges hblFreightsAndCharges = new HblFreightsAndCharges();
        hblFreightsAndCharges.setCharges("100");
        hblFreightsAndCharges.setChargeType("COLLECT");
        hblFreightsAndCharges.setValue(11.00);
        hbl.setHblFreightsAndCharges(List.of(hblFreightsAndCharges));

        Map<String, Object> dictionary = new HashMap<>();
        assertThrows(ValidationException.class,
                () -> hblReport.populateFreightsAndCharges(dictionary, hbl));
    }

    @Test
    void populateFreightsAndCharges_whenIsRatedBLTrueNullValue() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        HblFreightsAndCharges hblFreightsAndCharges = new HblFreightsAndCharges();
        hblFreightsAndCharges.setCharges("100");
        hblFreightsAndCharges.setChargeType("COLLECT");
        hblFreightsAndCharges.setCurrency("USD");
        hbl.setHblFreightsAndCharges(List.of(hblFreightsAndCharges));

        Map<String, Object> dictionary = new HashMap<>();
        assertThrows(ValidationException.class,
                () -> hblReport.populateFreightsAndCharges(dictionary, hbl));
    }

    @Test
    void populateFreightsAndCharges_whenIsRatedBLTrueNullCharges() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        HblFreightsAndCharges hblFreightsAndCharges = new HblFreightsAndCharges();
        hblFreightsAndCharges.setChargeType("COLLECT");
        hblFreightsAndCharges.setCurrency("USD");
        hbl.setHblFreightsAndCharges(List.of(hblFreightsAndCharges));

        Map<String, Object> dictionary = new HashMap<>();
        assertThrows(ValidationException.class,
                () -> hblReport.populateFreightsAndCharges(dictionary, hbl));
    }

    @Test
    void populateFreightsAndCharges_whenIsRatedBLTrueNullChargeType() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        HblFreightsAndCharges hblFreightsAndCharges = new HblFreightsAndCharges();
        hblFreightsAndCharges.setCharges("1001");
        hblFreightsAndCharges.setCurrency("USD");
        hblFreightsAndCharges.setValue(100.00);
        hbl.setHblFreightsAndCharges(List.of(hblFreightsAndCharges));

        Map<String, Object> dictionary = new HashMap<>();
        assertThrows(ValidationException.class,
                () -> hblReport.populateFreightsAndCharges(dictionary, hbl));
    }

    @Test
    void populateFreightsAndCharges_whenFreightsAndChargesIsNull_11() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);
        hbl.setHblFreightsAndCharges(null);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        Map<String, Object> dictionary = new HashMap<>();
        assertThrows(ValidationException.class,
                () -> hblReport.populateFreightsAndCharges(dictionary, hbl));
    }

    @Test
    void whenFreightsAndChargesIsNull_thenSkipProcessing() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);
        hbl.setHblFreightsAndCharges(null);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(new ShipmentDetails()));

        Map<String, Object> dictionary = new HashMap<>();
        hblReport.populateFreightsAndCharges(dictionary, hbl);

        assertTrue(dictionary.isEmpty());
    }

    @Test
    void whenFreightsAndChargesIsEmpty_thenSkipProcessing() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(1L);
        hbl.setHblFreightsAndCharges(Collections.emptyList());

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(new ShipmentDetails()));

        Map<String, Object> dictionary = new HashMap<>();
        hblReport.populateFreightsAndCharges(dictionary, hbl);

        assertTrue(dictionary.isEmpty());
    }

    @Test
    void populateFreightsAndCharges_whenRatedBLThrowsValidationError() {
        Hbl hbl = new Hbl();
        hbl.setShipmentId(4L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(4L)).thenReturn(Optional.of(shipmentDetails));
        hbl.setHblFreightsAndCharges(new ArrayList<>());

        Map<String, Object> dictionary = new HashMap<>();
        assertThrows(ValidationException.class,
                () -> hblReport.populateFreightsAndCharges(dictionary, hbl));
    }

    @Test
    void populateFreightsAndCharges_whenIsRatedBLFalse() {

        Hbl hbl = new Hbl();
        hbl.setShipmentId(2L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsRatedBL(false);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        when(shipmentDao.findById(2L)).thenReturn(Optional.of(shipmentDetails));
        Map<String, Object> dictionary = new HashMap<>();

        hblReport.populateFreightsAndCharges(dictionary, hbl);
        assertFalse(dictionary.isEmpty());
    }

    void setData(HblModel hblModel, Hbl hbl, ConsolidationModel consolidationModel) {


        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hbl.setHblData(hblDataDto);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        hblModel.setCommonContainers(Arrays.asList(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .partiesList(List.of(
                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("DAG").orgData(Map.of("FullName", "name", "ContactPhone","88")).addressData(Map.of()).build()
                ))
                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
                .transporterDetail(PartiesModel.builder().type("EXA").build())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
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
        ReferenceNumbersModel ernReferenceNumbersModel = new ReferenceNumbersModel();
        ernReferenceNumbersModel.setType(ERN);
        ReferenceNumbersModel cenReferenceNumbersModel = new ReferenceNumbersModel();
        cenReferenceNumbersModel.setType(CEN);
        ReferenceNumbersModel frnReferenceNumbersModel = new ReferenceNumbersModel();
        frnReferenceNumbersModel.setType(FRN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);

        PackingModel packingModel = new PackingModel();
        packingModel.setPacks("3");
        packingModel.setPacksType("BAG");
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));
        hblModel.setTransportInstructionId(12L);
        hblModel.setShipment(shipmentModel);

        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);
    }

    @ParameterizedTest
    @CsvSource({
            "'US MOCK PLACE', true, 'US001', '', ''",
            "'US MOCK PLACE', true, 'US001', 'XYZ', 'AB'",
            "'US MOCK PLACE', true, 'US001', '', 'AB'",
            "'US MOCK PLACE', true, 'US001', 'XYZ', ''",
            "'US MOCK PLACE', true, 'US001', 'XYZ', null",
            "'US MOCK PLACE', true, 'US001', null, 'AB'",
            "'Mumbai', true, 'GB001', '', ''",
            "'Mumbai', false, 'GB001', '', ''",
            "'', true, '', '', ''",
            "'Mumbai', true, '', 'ZBC', ''"
    })
    void populateDictionaryWithDifferentLocationAndRunnerV3(String placeOfIssue, boolean isRunnerV3Enabled,
                                                            String expectedLocCode, String cityName, String stateName) throws JsonProcessingException {

        // Setup ShipmentSettings with the varying RunnerV3Enabled flag
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetails.builder()
                .autoUpdateShipmentBL(true)
                .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                .isRunnerV3Enabled(isRunnerV3Enabled)
                .build();

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        // Setup HblModel and other common objects
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);

        Hbl hbl = new Hbl();
        hbl.setId(123L);
        hbl.setShipmentId(123L);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        setData(hblModel, hbl, consolidationModel);

        // Mock the shared methods (LocationData, VesselData, etc.)
        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);

        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");

        // Populating the EntityTransferUnLocations and UnlocationsResponse mocks
        EntityTransferUnLocations mockEntityTransferUnLocations = new EntityTransferUnLocations();
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = new HashMap<>();
        entityTransferUnLocationsMap.put(placeOfIssue, mockEntityTransferUnLocations);
        when(masterDataUtils.getLocationDataFromCache(anySet(), anyString()))
                .thenReturn(entityTransferUnLocationsMap);

        UnlocationsResponse mockUnlocationsResponse = new UnlocationsResponse();
        mockUnlocationsResponse.setName(placeOfIssue);
        mockUnlocationsResponse.setLocCode(expectedLocCode);
        mockUnlocationsResponse.setName(cityName);
        mockUnlocationsResponse.setCountry(stateName);
        when(jsonHelper.convertValue(any(EntityTransferUnLocations.class), eq(UnlocationsResponse.class)))
                .thenReturn(mockUnlocationsResponse);

        // Mock the JSON and other related methods
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(new StringBuilder());

        // Setup Additional Details based on the PlaceOfIssue value
        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
        additionalDetails.setPlaceOfIssue(placeOfIssue);
        hblModel.shipment.setAdditionalDetails(additionalDetails);

        EntityTransferAddress entityAddress = new EntityTransferAddress();
        entityAddress.setCountry("India");
        when(commonUtils.getEntityTransferAddress(hblModel.shipment.getTransportMode())).thenReturn(entityAddress);

        // Perform the actual test
        assertNotNull(hblReport.populateDictionary(hblModel));

        // Optionally, you could add assertions to check if the locCode was properly processed.
        // For instance:
        assertEquals(expectedLocCode, mockUnlocationsResponse.getLocCode());
    }

    @Test
    void populateDictionaryWithIsHblTrue() throws JsonProcessingException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsRunnerV3Enabled(true);
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(true);
        UsersDto usersDto = new UsersDto();
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hblDataDto.setMarksAndNumbers("123");
        hbl.setHblData(hblDataDto);
        hblModel.setBlObject(hbl);
        Map<String, Long> containerGroupMap = new HashMap<>();
        containerGroupMap.put("TEST", 40L);
        containerGroupMap.put("TEST2", 40L);
        hblModel.setContainerCountGrouped(containerGroupMap);
        hblModel.setContainerPacksGrouped(containerGroupMap);
        Map<String, Double> volumeGroupMap = new HashMap<>();
        volumeGroupMap.put("TEST", 40.1);
        volumeGroupMap.put("TEST2", 40.1);
        hblModel.setContainerVolumeGrouped(volumeGroupMap);
        hblModel.setContainerWeightGrouped(volumeGroupMap);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        hblModel.setShipmentSettingsDetails(ShipmentSettingsDetails.builder().build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("INR");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("INR");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setGoodsDescription("DESC");
        shipmentModel.setMarksNum("Marks");
        shipmentModel.setNoOfPacks(10);
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        additionalDetailModel.setSendingAgent(partiesModel);
        additionalDetailModel.setReceivingAgent(partiesModel);
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hblModel.setShipment(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("123");
        packingModel.setHazardous(true);
        packingModel.setIsTemperatureControlled(true);
        packingModel.setWeight(BigDecimal.TEN);
        packingModel.setVolume(BigDecimal.TEN);
        packingModel.setWeightUnit("KG");
        packingModel.setVolumeUnit("M3");
        packingModel.setPacks("2");
        packingModel.setPacksType("Bag");
        packingModel.setContainerId(22L);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        ContainerModel containerModel = new ContainerModel();
        containerModel.setId(22L);
        containerModel.setContainerCode("20GP");
        containerModel.setContainerNumber("CONT3233323");
        containerModel.setContainerCount(1L);
        containerModel.setGrossWeight(BigDecimal.TEN);
        containerModel.setGrossVolume(BigDecimal.TEN);
        containerModel.setGrossWeightUnit("KG");
        containerModel.setGrossVolumeUnit("M3");
        containerModel.setDescriptionOfGoods("DESC");
        containerModel.setMarksNums("MarksNums");
        shipmentModel.setContainersList(Arrays.asList(containerModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(new HashMap<>());
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(masterDataUtils.fetchMasterListFromCache(any())).thenReturn(new HashMap<>());


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(any())).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(hblReport.populateDictionary(hblModel));
    }

    @Test
    void populateDictionaryWithIsHblTrue_LCL() throws JsonProcessingException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsRunnerV3Enabled(true);
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(true);
        UsersDto usersDto = new UsersDto();
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hblDataDto.setMarksAndNumbers("123");
        hbl.setHblData(hblDataDto);
        hblModel.setBlObject(hbl);
        Map<String, Long> containerGroupMap = new HashMap<>();
        containerGroupMap.put("TEST", 40L);
        containerGroupMap.put("TEST2", 40L);
        hblModel.setContainerCountGrouped(containerGroupMap);
        hblModel.setContainerPacksGrouped(containerGroupMap);
        Map<String, Double> volumeGroupMap = new HashMap<>();
        volumeGroupMap.put("TEST", 40.1);
        volumeGroupMap.put("TEST2", 40.1);
        hblModel.setContainerVolumeGrouped(volumeGroupMap);
        hblModel.setContainerWeightGrouped(volumeGroupMap);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        hblModel.setShipmentSettingsDetails(ShipmentSettingsDetails.builder().build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("INR");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("INR");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setGoodsDescription("DESC");
        shipmentModel.setMarksNum("Marks");
        shipmentModel.setNoOfPacks(10);
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        additionalDetailModel.setSendingAgent(partiesModel);
        additionalDetailModel.setReceivingAgent(partiesModel);
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hblModel.setShipment(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("123");
        packingModel.setHazardous(true);
        packingModel.setIsTemperatureControlled(true);
        packingModel.setPacks("2");
        packingModel.setPacksType("Bag");
        packingModel.setContainerId(22L);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        ContainerModel containerModel = new ContainerModel();
        containerModel.setId(22L);
        containerModel.setContainerCode("20GP");
        containerModel.setContainerNumber("CONT3233323");
        containerModel.setContainerCount(1L);
        containerModel.setGrossWeight(BigDecimal.TEN);
        containerModel.setGrossVolume(BigDecimal.TEN);
        containerModel.setGrossWeightUnit("KG");
        containerModel.setGrossVolumeUnit("M3");
        containerModel.setDescriptionOfGoods("DESC");
        containerModel.setMarksNums("MarksNums");
        shipmentModel.setContainersList(Arrays.asList(containerModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(new HashMap<>());
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(masterDataUtils.fetchMasterListFromCache(any())).thenReturn(new HashMap<>());


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(any())).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(hblReport.populateDictionary(hblModel));
    }

    @Test
    void populateDictionary_dgCheck() throws JsonProcessingException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hbl.setHblData(hblDataDto);
        hblModel.setBlObject(hbl);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        hblModel.setCommonContainers(Arrays.asList(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setTransportMode(AIR);
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
        shipmentModel.setIsNotifyConsigneeEqual(true);
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hblModel.setShipment(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setHazardous(true);
        packingModel.setPacks("3");
        packingModel.setPacksType("BBK");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        consolidationModel.setHazardous(true);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        consolidationDetails.setHazardous(true);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        Map<String, Object> response = hblReport.populateDictionary(hblModel);
        assertNotNull(response);
    }

    @Test
    void populateDictionary_dgCheck_failure() throws JsonProcessingException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hbl.setHblData(hblDataDto);
        hblModel.setBlObject(hbl);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        hblModel.setCommonContainers(Arrays.asList(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setTransportMode(AIR);
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
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hblModel.setShipment(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> hblReport.populateDictionary(hblModel));
    }

    @Test
    void populateDictionaryWithIsSelectedForDocumentCheck() throws JsonProcessingException {
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hblDataDto.setPlaceOfDelivery("deliveryAddress");
        hbl.setHblData(hblDataDto);
        hblModel.setBlObject(hbl);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        hblModel.setCommonContainers(List.of(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(List.of(hblPartyDto));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .partiesList(List.of(
                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("DAG").orgData(Map.of("FullName", "name", "ContactPhone","88")).addressData(Map.of()).build()
                ))
                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
                .transporterDetail(PartiesModel.builder().type("EXA").build())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
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
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(List.of(referenceNumbersModel));
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setVesselName("VesselName");
        routingsModel.setFlightNumber("FlightNumber");
        routingsModel.setIsSelectedForDocument(true);
        routingsModel.setVoyage("voyage");

        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_SEA);
        routingsModel2.setCarrier("test2");
        routingsModel2.setVesselName("VesselName2");
        routingsModel2.setFlightNumber("FlightNumber2");
        routingsModel2.setIsSelectedForDocument(false);
        routingsModel2.setVoyage("voyage2");
        shipmentModel.setRoutingsList(Arrays.asList(routingsModel, routingsModel2));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(List.of(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(List.of(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setPacks("4");
        packingModel.setPacksType("BAG");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(List.of(bookingCarriageModel));

        ShipmentOrderModel shipmentOrderModel = new ShipmentOrderModel();
        shipmentOrderModel.setOrderNumber("1234-5678-9123-4567");

        ShipmentOrderModel shipmentOrderModel2 = new ShipmentOrderModel();
        shipmentOrderModel2.setOrderNumber("1235-5678-9123-4567");

        ShipmentOrderModel shipmentOrderModel3 = new ShipmentOrderModel();
        shipmentOrderModel3.setOrderNumber("1235-5679-9123-4567");

        shipmentModel.setShipmentOrders(Arrays.asList(shipmentOrderModel, shipmentOrderModel2, shipmentOrderModel3));

        hblModel.setTransportInstructionId(12L);
        hblModel.setShipment(shipmentModel);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(List.of(partiesModel));
        consolidationModel.setReferenceNumbersList(List.of(referenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(List.of(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        mockTenantSettings();
        Map<String, Object> dict = hblReport.populateDictionary(hblModel);
        assertNotNull(dict);
        assertNotNull(dict.get(ReportConstants.ORDER_MANAGEMENT_NUMBER));
        assertEquals("1234-5678-9123-4567,1235-5678-9123-4567,1235-5679-9123-4567", dict.get(ReportConstants.ORDER_MANAGEMENT_NUMBER));
    }

    @Test
    void populateDictionaryWithShipmentContainers() throws JsonProcessingException {
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode("transportMode");
        shipmentModel.setGoodsDescription("GoodsDescription");
        hblModel.setShipment(shipmentModel);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hbl.setHblData(hblDataDto);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerNumber("123");
        shipmentContainers.setShipmentMarksnNums("235");
        shipmentContainers.setShipmentPacksUnit("Unit");
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setContainerTypeCode("Type");
        shipmentContainers.setDescriptionOfGoods("description");
        shipmentContainers.setCarrierSealNumber("CarrierSeal");
        shipmentContainers.setCustomsSealNumber("CustomerSealNumber");
        shipmentContainers.setShipperSealNumber("Sealnumber");
        hblModel.setCommonContainers(List.of(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(List.of(hblPartyDto));
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .partiesList(List.of(
                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("DAG").orgData(Map.of("FullName", "name", "ContactPhone","88")).addressData(Map.of()).build()
                ))
                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
                .transporterDetail(PartiesModel.builder().type("EXA").build())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
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
        ReferenceNumbersModel ernReferenceNumbersModel = new ReferenceNumbersModel();
        ernReferenceNumbersModel.setType(ERN);
        ReferenceNumbersModel cenReferenceNumbersModel = new ReferenceNumbersModel();
        cenReferenceNumbersModel.setType(CEN);
        ReferenceNumbersModel frnReferenceNumbersModel = new ReferenceNumbersModel();
        frnReferenceNumbersModel.setType(FRN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setVesselName("VesselName");
        routingsModel.setFlightNumber("FlightNumber");
        routingsModel.setIsSelectedForDocument(true);
        routingsModel.setVoyage("voyage");

        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_SEA);
        routingsModel2.setCarrier("test2");
        routingsModel2.setVesselName("VesselName2");
        routingsModel2.setFlightNumber("FlightNumber2");
        routingsModel2.setIsSelectedForDocument(false);
        routingsModel2.setVoyage("voyage2");
        shipmentModel.setRoutingsList(Arrays.asList(routingsModel, routingsModel2));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(List.of(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(List.of(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setPacks("4");
        packingModel.setPacksType("BAG");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(List.of(bookingCarriageModel));
        hblModel.setTransportInstructionId(12L);
        hblModel.setShipment(shipmentModel);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(List.of(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(List.of(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(hblReport.populateDictionary(hblModel));
    }

    @Test
    void populateDictionaryWithNoBLObject() throws JsonProcessingException {
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(false);
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode("transportMode");
        shipmentModel.setGoodsDescription("GoodsDescription");
        hblModel.setShipment(shipmentModel);
        UsersDto usersDto = new UsersDto();
        usersDto.setHouseBillLogo("123");
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
        hbl.setHblData(hblDataDto);
        hblModel.setTenant(new TenantModel());
        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerNumber("123");
        shipmentContainers.setShipmentMarksnNums("235");
        shipmentContainers.setShipmentPacksUnit("Unit");
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setContainerTypeCode("Type");
        shipmentContainers.setDescriptionOfGoods("description");
        shipmentContainers.setCarrierSealNumber("CarrierSeal");
        shipmentContainers.setCustomsSealNumber("CustomerSealNumber");
        shipmentContainers.setShipperSealNumber("Sealnumber");
        hblModel.setCommonContainers(List.of(shipmentContainers));

        HblPartyDto hblPartyDto = new HblPartyDto();
        hbl.setHblNotifyParty(List.of(hblPartyDto));
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .partiesList(List.of(
                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
                        PartiesModel.builder().type("DAG").orgData(Map.of("FullName", "name", "ContactPhone","88")).addressData(Map.of()).build()
                ))
                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
                .transporterDetail(PartiesModel.builder().type("EXA").build())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
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
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(List.of(referenceNumbersModel));
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setVesselName("VesselName");
        routingsModel.setFlightNumber("FlightNumber");
        routingsModel.setIsSelectedForDocument(true);
        routingsModel.setVoyage("voyage");

        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_SEA);
        routingsModel2.setCarrier("test2");
        routingsModel2.setVesselName("VesselName2");
        routingsModel2.setFlightNumber("FlightNumber2");
        routingsModel2.setIsSelectedForDocument(false);
        routingsModel2.setVoyage("voyage2");
        shipmentModel.setRoutingsList(Arrays.asList(routingsModel, routingsModel2));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(List.of(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(List.of(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        shipmentModel.setGoodsDescription("Description");

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setPacks("4");
        packingModel.setPacksType("BAG");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(List.of(bookingCarriageModel));
        hblModel.setTransportInstructionId(12L);
        hblModel.setShipment(shipmentModel);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(List.of(partiesModel));
        consolidationModel.setReferenceNumbersList(List.of(referenceNumbersModel));
        hblModel.setConsolidation(consolidationModel);

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(List.of(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        String blObjectJson = objectMapper.writeValueAsString(dataMap);
        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(hblReport.populateDictionary(hblModel));
    }
//    @Test
//    void populateDictionaryfordeliveryForwardAgent() throws JsonProcessingException {
//        HblModel hblModel = new HblModel();
//        hblModel.setIsHbl(false);
//        UsersDto usersDto = new UsersDto();
//        usersDto.setHouseBillLogo("123");
//        hblModel.setUser(usersDto);
//        Hbl hbl = new Hbl();
//        hbl.setId(123L);
//        hbl.setShipmentId(123L);
//
//        HblDataDto hblDataDto = new HblDataDto();
//        hblDataDto.setMarksAndNumbers("123");
//        hblDataDto.setPlaceOfDelivery("deliveryAddress");
//        // Add new fields for consignor and consignee
//        hblDataDto.setConsignorName("Consignor Name");
//        hblDataDto.setConsignorAddress("Consignor Address");
//        hblDataDto.setConsigneeName("Consignee Name");
//        hblDataDto.setConsigneeAddress("Consignee Address");
//        // Add fields for delivery and forward agent
//        hblDataDto.setDeliveryAgentName("Delivery Agent Name");
//        hblDataDto.setDeliveryAgentAddress("Delivery Agent Address");
//        hbl.setHblData(hblDataDto);
//        hblModel.setBlObject(hbl); // Set the blObject
//        hblModel.setTenant(new TenantModel());
//        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
//        ShipmentContainers shipmentContainers = new ShipmentContainers();
//        shipmentContainers.setContainerCount(1L);
//        shipmentContainers.setContainerTypeCode("20GP");
//        shipmentContainers.setNetWeight(BigDecimal.TEN);
//        shipmentContainers.setNoofPackages(10L);
//        hblModel.setCommonContainers(Arrays.asList(shipmentContainers));
//
//        HblPartyDto hblPartyDto = new HblPartyDto();
//        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
//        ShipmentModel shipmentModel = new ShipmentModel();
//        shipmentModel.setTransportInstructionId(12L);
//        shipmentModel.setTransportMode(ReportConstants.SEA);
//        shipmentModel.setTransportInstructionId(12L);
//
//        // Create delivery and forward agent parties for pickup/delivery details
//        PartiesModel deliveryAgentParty = PartiesModel.builder()
//                .type("DAG")
//                .orgData(Map.of("FullName", "Delivery Agent", "ContactPhone", "123-456-7890"))
//                .addressData(Map.of("AddressLine1", "Delivery Agent Address"))
//                .build();
//
//        PartiesModel forwardAgentParty = PartiesModel.builder()
//                .type("FAG")
//                .orgData(Map.of("FullName", "Forward Agent", "ContactPhone", "987-654-3210"))
//                .addressData(Map.of("AddressLine1", "Forward Agent Address"))
//                .build();
//
//        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
//                .id(12L)
//                .partiesList(List.of(
//                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
//                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
//                        deliveryAgentParty, // Add delivery agent
//                        forwardAgentParty  // Add forward agent
//                ))
//                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
//                .transporterDetail(PartiesModel.builder().type("EXA").build())
//                .actualPickup(LocalDateTime.now())
//                .actualDelivery(LocalDateTime.now())
//                .build()));
//        shipmentModel.setDirection(ReportConstants.EXP);
//        shipmentModel.setFreightLocal(BigDecimal.TEN);
//        shipmentModel.setFreightLocalCurrency("INR");
//        shipmentModel.setFreightOverseas(BigDecimal.TEN);
//        shipmentModel.setFreightOverseasCurrency("INR");
//        shipmentModel.setGoodsDescription("123");
//        shipmentModel.setWeight(BigDecimal.TEN);
//        shipmentModel.setVolume(BigDecimal.TEN);
//        shipmentModel.setChargable(BigDecimal.TEN);
//        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
//        shipmentModel.setNoOfPacks(10);
//        ReferenceNumbersModel ernReferenceNumbersModel = new ReferenceNumbersModel();
//        ernReferenceNumbersModel.setType(ERN);
//        ReferenceNumbersModel cenReferenceNumbersModel = new ReferenceNumbersModel();
//        cenReferenceNumbersModel.setType(CEN);
//        ReferenceNumbersModel frnReferenceNumbersModel = new ReferenceNumbersModel();
//        frnReferenceNumbersModel.setType(FRN);
//        shipmentModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
//
//        PartiesModel partiesModel = new PartiesModel();
//        partiesModel.setType(CUSTOM_HOUSE_AGENT);
//        Map<String, Object> orgData = new HashMap<>();
//        orgData.put(FULL_NAME, "123");
//        orgData.put(CONTACT_PERSON, "123");
//        partiesModel.setOrgData(orgData);
//        partiesModel.setAddressData(orgData);
//        shipmentModel.setConsignee(partiesModel);
//        shipmentModel.setConsigner(partiesModel);
//        shipmentModel.setClient(partiesModel);
//        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
//        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
//        carrierDetailModel.setOrigin("test");
//        carrierDetailModel.setOriginPort("test");
//        carrierDetailModel.setEta(LocalDateTime.now());
//        carrierDetailModel.setEtd(LocalDateTime.now());
//        carrierDetailModel.setAtd(LocalDateTime.now());
//        carrierDetailModel.setVessel(UUID.randomUUID().toString());
//        carrierDetailModel.setAta(LocalDateTime.now());
//        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
//        additionalDetailModel.setPaidPlace("test");
//        additionalDetailModel.setNotifyParty(partiesModel);
//        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
//        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
//        additionalDetailModel.setOnBoard("SHP");
//        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
//        shipmentModel.setCarrierDetails(carrierDetailModel);
//        shipmentModel.setAdditionalDetails(additionalDetailModel);
//        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));
//
//        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
//        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
//        delivertDetails.setDestinationDetail(partiesModel);
//        delivertDetails.setAgentDetail(deliveryAgentParty); // Set delivery agent
//        delivertDetails.setSourceDetail(partiesModel);
//        delivertDetails.setTransporterDetail(partiesModel);
//        shipmentModel.setPickupDetails(delivertDetails);
//        shipmentModel.setDeliveryDetails(delivertDetails);
//
//        PackingModel packingModel = new PackingModel();
//        packingModel.setLength(BigDecimal.TEN);
//        packingModel.setWidth(BigDecimal.TEN);
//        packingModel.setHeight(BigDecimal.TEN);
//        shipmentModel.setPackingList(Arrays.asList(packingModel));
//
//        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
//        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
//        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));
//        hblModel.setTransportInstructionId(12L);
//        hblModel.setShipment(shipmentModel);
//
//        ConsolidationModel consolidationModel = new ConsolidationModel();
//        consolidationModel.setPayment("PPM");
//        consolidationModel.setReceivingAgent(partiesModel);
//        consolidationModel.setSendingAgent(partiesModel);
//        consolidationModel.setCarrierDetails(carrierDetailModel);
//        partiesModel = new PartiesModel();
//        partiesModel.setType("Notify Party 1");
//        orgData = new HashMap<>();
//        orgData.put(FULL_NAME, "123");
//        partiesModel.setOrgData(orgData);
//        partiesModel.setAddressData(orgData);
//        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
//        consolidationModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
//        hblModel.setConsolidation(consolidationModel);
//
//        // Mock shipment settings with disableBlPartiesName
//        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
//        shipmentSettingsDetails.setDisableBlPartiesName(false); // Set appropriate value for test
//        hblModel.setShipmentSettingsDetails(shipmentSettingsDetails);
//
//        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());
//
//        V1DataResponse v1DataResponse = new V1DataResponse();
//        v1DataResponse.entities = Arrays.asList(new MasterData());
//        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));
//
//        v1DataResponse = new V1DataResponse();
//        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
//        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());
//
//        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
//        consoleShipmentMapping.setShipmentId(1L);
//        consoleShipmentMapping.setConsolidationId(1L);
//        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
//        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
//        consolidationDetails.setId(123L);
//        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
//        Map<String, Object> dataMap = new HashMap<>();
//        dataMap.put("id", "123");
//        Map<String, Object> dictionary = new HashMap<>();
//        dictionary.put("id", "123");
//        String blObjectJson = objectMapper.writeValueAsString(dataMap);
//        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
//        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
//        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
//        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
//        mockShipmentSettings();
//        mockTenantSettings();
//        when(cacheManager.getCache(any())).thenReturn(cache);
//        when(cache.get(any())).thenReturn(null);
//        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
//        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
//        additionalDetails.setPlaceOfIssue("mumbai");
//        hblModel.shipment.setAdditionalDetails(additionalDetails);
//        EntityTransferAddress entityAddress = new EntityTransferAddress();
//        entityAddress.setCountry("India");
//        when(commonUtils.getEntityTransferAddress(hblModel.shipment.getTransportMode())).thenReturn(entityAddress);
//        assertNotNull(hblReport.populateDictionary(hblModel));
//    }

//
//    @Test
//    void populateDictionaryforConsigneeConsignor() throws JsonProcessingException {
//        HblModel hblModel = new HblModel();
//        hblModel.setIsHbl(false);
//        UsersDto usersDto = new UsersDto();
//        usersDto.setHouseBillLogo("123");
//        hblModel.setUser(usersDto);
//        Hbl hbl = new Hbl();
//        hbl.setId(123L);
//        hbl.setShipmentId(123L);
//
//        HblDataDto hblDataDto = new HblDataDto();
//        hblDataDto.setMarksAndNumbers("123");
//        hblDataDto.setPlaceOfDelivery("deliveryAddress");
//        // Add new fields for consignor and consignee
//        hblDataDto.setConsignorName("Consignor Name");
//        hblDataDto.setConsignorAddress("Consignor Address");
//        hblDataDto.setConsigneeName("Consignee Name");
//        hblDataDto.setConsigneeAddress("Consignee Address");
//        // Add fields for delivery and forward agent
//        hblDataDto.setDeliveryAgentName("Delivery Agent Name");
//        hblDataDto.setDeliveryAgentAddress("Delivery Agent Address");
//        hbl.setHblData(hblDataDto);
//        hblModel.setBlObject(hbl); // Set the blObject
//        hblModel.setTenant(new TenantModel());
//        hblModel.setTenantSettingsResponse(V1TenantSettingsResponse.builder().P100Branch(false).build());
//        ShipmentContainers shipmentContainers = new ShipmentContainers();
//        shipmentContainers.setContainerCount(1L);
//        shipmentContainers.setContainerTypeCode("20GP");
//        shipmentContainers.setNetWeight(BigDecimal.TEN);
//        shipmentContainers.setNoofPackages(10L);
//        hblModel.setCommonContainers(Arrays.asList(shipmentContainers));
//
//        HblPartyDto hblPartyDto = new HblPartyDto();
//        hbl.setHblNotifyParty(Arrays.asList(hblPartyDto));
//        ShipmentModel shipmentModel = new ShipmentModel();
//        shipmentModel.setTransportInstructionId(12L);
//        shipmentModel.setTransportMode(ReportConstants.SEA);
//        shipmentModel.setTransportInstructionId(12L);
//
//        // Create delivery and forward agent parties for pickup/delivery details
//        PartiesModel consignerParty = PartiesModel.builder()
//                .type("CNE")   // or whatever enum/string your code expects
//                .orgData(Map.of(
//                        "FullName",       "Consignor Name",
//                        "ContactPerson",  "John Doe",
//                        "ContactPhone",   "111-222-3333",
//                        "Email",          "consignor@example.com"
//                ))
//                .addressData(Map.of(
//                        "AddressLine1", "Consignor Street 1",
//                        "AddressLine2", "Suite 101",
//                        "City",         "Mumbai",
//                        "Country",      "India"
//                ))
//                .build();
//
//// --- CONSIGNEE ---
//        PartiesModel consigneeParty = PartiesModel.builder()
//                .type("CNEE")
//                .orgData(Map.of(
//                        "FullName",       "Consignee Name",
//                        "ContactPerson",  "Jane Smith",
//                        "ContactPhone",   "444-555-6666",
//                        "Email",          "consignee@example.com"
//                ))
//                .addressData(Map.of(
//                        "AddressLine1", "Consignee Lane 10",
//                        "AddressLine2", "Warehouse Block B",
//                        "City",         "Chennai",
//                        "Country",      "India"
//                ))
//                .build();
//
//        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
//                .id(12L)
//                .partiesList(List.of(
//                        PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build(),
//                        PartiesModel.builder().type("IMA").orgData(Map.of("FullName", "name", "ContactPhone", "99")).addressData(Map.of()).build(),
//                        consigneeParty, // Add delivery agent
//                        consignerParty  // Add forward agent
//                ))
//                .sourceDetail(PartiesModel.builder().type("EXA").orgData(Map.of("FullName", "name", "ContactPhone" , "88")).addressData(Map.of()).build())
//                .transporterDetail(PartiesModel.builder().type("EXA").build())
//                .actualPickup(LocalDateTime.now())
//                .actualDelivery(LocalDateTime.now())
//                .build()));
//        shipmentModel.setDirection(ReportConstants.EXP);
//        shipmentModel.setFreightLocal(BigDecimal.TEN);
//        shipmentModel.setFreightLocalCurrency("INR");
//        shipmentModel.setFreightOverseas(BigDecimal.TEN);
//        shipmentModel.setFreightOverseasCurrency("INR");
//        shipmentModel.setGoodsDescription("123");
//        shipmentModel.setWeight(BigDecimal.TEN);
//        shipmentModel.setVolume(BigDecimal.TEN);
//        shipmentModel.setChargable(BigDecimal.TEN);
//        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
//        shipmentModel.setNoOfPacks(10);
//        ReferenceNumbersModel ernReferenceNumbersModel = new ReferenceNumbersModel();
//        ernReferenceNumbersModel.setType(ERN);
//        ReferenceNumbersModel cenReferenceNumbersModel = new ReferenceNumbersModel();
//        cenReferenceNumbersModel.setType(CEN);
//        ReferenceNumbersModel frnReferenceNumbersModel = new ReferenceNumbersModel();
//        frnReferenceNumbersModel.setType(FRN);
//        shipmentModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
//
//        PartiesModel partiesModel = new PartiesModel();
//        partiesModel.setType(CUSTOM_HOUSE_AGENT);
//        Map<String, Object> orgData = new HashMap<>();
//        orgData.put(FULL_NAME, "123");
//        orgData.put(CONTACT_PERSON, "123");
//        partiesModel.setOrgData(orgData);
//        partiesModel.setAddressData(orgData);
//        shipmentModel.setConsignee(partiesModel);
//        shipmentModel.setConsigner(partiesModel);
//        shipmentModel.setClient(partiesModel);
//        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
//        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
//        carrierDetailModel.setOrigin("test");
//        carrierDetailModel.setOriginPort("test");
//        carrierDetailModel.setEta(LocalDateTime.now());
//        carrierDetailModel.setEtd(LocalDateTime.now());
//        carrierDetailModel.setAtd(LocalDateTime.now());
//        carrierDetailModel.setVessel(UUID.randomUUID().toString());
//        carrierDetailModel.setAta(LocalDateTime.now());
//        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
//        additionalDetailModel.setPaidPlace("test");
//        additionalDetailModel.setNotifyParty(partiesModel);
//        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
//        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
//        additionalDetailModel.setOnBoard("SHP");
//        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
//        shipmentModel.setCarrierDetails(carrierDetailModel);
//        shipmentModel.setAdditionalDetails(additionalDetailModel);
//        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));
//
//        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
//        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
//        delivertDetails.setDestinationDetail(partiesModel);
//        delivertDetails.setAgentDetail(consigneeParty); // Set delivery agent
//        delivertDetails.setSourceDetail(partiesModel);
//        delivertDetails.setTransporterDetail(partiesModel);
//        shipmentModel.setPickupDetails(delivertDetails);
//        shipmentModel.setDeliveryDetails(delivertDetails);
//
//        PackingModel packingModel = new PackingModel();
//        packingModel.setLength(BigDecimal.TEN);
//        packingModel.setWidth(BigDecimal.TEN);
//        packingModel.setHeight(BigDecimal.TEN);
//        shipmentModel.setPackingList(Arrays.asList(packingModel));
//
//        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
//        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
//        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));
//        hblModel.setTransportInstructionId(12L);
//        hblModel.setShipment(shipmentModel);
//
//        ConsolidationModel consolidationModel = new ConsolidationModel();
//        consolidationModel.setPayment("PPM");
//        consolidationModel.setReceivingAgent(partiesModel);
//        consolidationModel.setSendingAgent(partiesModel);
//        consolidationModel.setCarrierDetails(carrierDetailModel);
//        partiesModel = new PartiesModel();
//        partiesModel.setType("Notify Party 1");
//        orgData = new HashMap<>();
//        orgData.put(FULL_NAME, "123");
//        partiesModel.setOrgData(orgData);
//        partiesModel.setAddressData(orgData);
//        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
//        consolidationModel.setReferenceNumbersList(Arrays.asList(ernReferenceNumbersModel,cenReferenceNumbersModel,frnReferenceNumbersModel));
//        hblModel.setConsolidation(consolidationModel);
//
//        // Mock shipment settings with disableBlPartiesName
//        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
//        shipmentSettingsDetails.setDisableBlPartiesName(false); // Set appropriate value for test
//        hblModel.setShipmentSettingsDetails(shipmentSettingsDetails);
//
//        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());
//
//        V1DataResponse v1DataResponse = new V1DataResponse();
//        v1DataResponse.entities = Arrays.asList(new MasterData());
//        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));
//
//        v1DataResponse = new V1DataResponse();
//        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
//        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());
//
//        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
//        consoleShipmentMapping.setShipmentId(1L);
//        consoleShipmentMapping.setConsolidationId(1L);
//        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
//        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
//        consolidationDetails.setId(123L);
//        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
//        Map<String, Object> dataMap = new HashMap<>();
//        dataMap.put("id", "123");
//        Map<String, Object> dictionary = new HashMap<>();
//        dictionary.put("id", "123");
//        String blObjectJson = objectMapper.writeValueAsString(dataMap);
//        when(jsonHelper.convertToJson(hbl)).thenReturn(blObjectJson);
//        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
//        when(jsonHelper.convertJsonToMap(blObjectJson)).thenReturn(dataMap);
//        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);
//        mockShipmentSettings();
//        mockTenantSettings();
//        when(cacheManager.getCache(any())).thenReturn(cache);
//        when(cache.get(any())).thenReturn(null);
//        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
//        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
//        additionalDetails.setPlaceOfIssue("mumbai");
//        hblModel.shipment.setAdditionalDetails(additionalDetails);
//        EntityTransferAddress entityAddress = new EntityTransferAddress();
//        entityAddress.setCountry("India");
//        when(commonUtils.getEntityTransferAddress(hblModel.shipment.getTransportMode())).thenReturn(entityAddress);
//        assertNotNull(hblReport.populateDictionary(hblModel));
//    }


}
