package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
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
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IHblDao hblDao;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

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
    void getDocumentModelWithoutBlObject() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
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
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
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

        when(masterDataUtils.getLocationData(any())).thenReturn(locationMap);
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

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(v1Service.fetchVesselData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), VesselsResponse.class)).thenReturn(Arrays.asList(new VesselsResponse()));
        mockShipmentSettings();
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

        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setMarksAndNumbers("123");
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

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(v1Service.fetchVesselData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), VesselsResponse.class)).thenReturn(Arrays.asList(new VesselsResponse()));

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
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
        assertNotNull(hblReport.populateDictionary(hblModel));
    }

    @Test
    void populateDictionaryWithIsHblTrue() throws JsonProcessingException {
        HblModel hblModel = new HblModel();
        hblModel.setIsHbl(true);
        UsersDto usersDto = new UsersDto();
        hblModel.setUser(usersDto);
        Hbl hbl = new Hbl();
        hbl.setId(123L);

        HblDataDto hblDataDto = new HblDataDto();
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
        shipmentModel.setPackingList(Arrays.asList(packingModel));

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

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(Arrays.asList(new MasterData()));


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(v1Service.fetchVesselData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), VesselsResponse.class)).thenReturn(Arrays.asList(new VesselsResponse()));

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
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
        when(v1Service.fetchVesselData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), VesselsResponse.class)).thenReturn(Arrays.asList(new VesselsResponse()));

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(Arrays.asList(consoleShipmentMapping));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        consolidationDetails.setHazardous(true);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
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
}
