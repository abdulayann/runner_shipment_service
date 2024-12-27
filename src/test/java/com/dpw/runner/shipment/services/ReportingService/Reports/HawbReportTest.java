package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbNotifyPartyInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbOtherInfo;
import com.dpw.runner.shipment.services.dto.request.reportService.CompanyDto;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
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
import org.modelmapper.ModelMapper;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HawbReportTest extends CommonMocks {

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @InjectMocks
    private HawbReport hawbReport;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private IMasterDataService iMasterDataService;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IAwbRepository awbRepository;

    @Mock
    private IAwbService awbService;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private IAwbDao awbDao;

    @Mock
    private HawbModel hawbModel;

    @Mock
    private AwbCargoInfo cargoInfoRows;

    @Mock
    private V1TenantSettingsResponse v1TenantSettingsResponse;

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

    Awb hawb;
    Awb mawb;
    ShipmentDetails shipmentDetails;
    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        hawb = jsonTestUtility.getTestHawb();
        mawb = jsonTestUtility.getTestMawb();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).DPWDateFormat("yyyy-MM-dd").build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        UsersDto usersDto = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.airDG, true);
        usersDto.setPermissions(permissions);
        UserContext.setUser(usersDto);
    }

    @Test
    void populateDictionary2() {
        HawbModel hawbModel = new HawbModel();
        hawbModel.setEntityType(HAWB);

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.AIR);
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

        List<ReferenceNumbersModel>  referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(CEN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(FRN);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

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
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setShippingLine("AIR lINE");
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("AIR");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
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

        hawbModel.setShipmentDetails(shipmentModel);
        String csdInfo = "some info";
        hawb.getAwbCargoInfo().setCsdInfo(csdInfo);
        hawb.setOriginalPrintedAt(LocalDateTime.now());
        hawb.getAwbCargoInfo().setOtherInfoCode("test");
        AwbOtherInfo otherInfo = hawb.getAwbOtherInfo();
        otherInfo.setCarrierName("carrierName");
        otherInfo.setCarrierHqAddress("hqAddress");
        otherInfo.setLegalCompanyName("legal");
        otherInfo.setAddress1("address1");
        otherInfo.setAddress2("address2");
        otherInfo.setState("state");
        otherInfo.setCity("city");
        otherInfo.setCountryCode("IN");
        otherInfo.setCountryName("India");
        otherInfo.setBranch("branch");
        hawb.setAwbOtherInfo(otherInfo);
        hawbModel.setAwb(hawb);
        UsersDto usersDto = new UsersDto();
        usersDto.setUsername("UserName");
        usersDto.setCompanyId(1);
        hawbModel.setUsersDto(usersDto);
        hawbModel.getAwb().setAwbNotifyPartyInfo(List.of(AwbNotifyPartyInfo.builder().name("Hello").address("test address").build()));
        hawbModel.getAwb().getAwbShipmentInfo().setAccountNumber("123");
        hawbModel.getAwb().getAwbShipmentInfo().setShipperAccountNumber("456");
        hawbModel.getAwb().getAwbShipmentInfo().setConsigneeAccountNumber("789");

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        TenantModel tenantModel = new TenantModel();
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        v1TenantSettingsResponse.setLegalEntityCode("EntityCode");
        CompanyDto companyDto = new CompanyDto();
        companyDto.setId(12);
        companyDto.setCountry("India");
        companyDto.setCompanyName("CompanyName");
        companyDto.setCity("city");
        companyDto.setCode("code");
        companyDto.setAddress1("Address1");
        companyDto.setAddress2("Address2");
        companyDto.setZipPostCode("ZipCode");
        List<CompanyDto> companyDtoList = new ArrayList<>();
        companyDtoList.add(companyDto);
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtoList);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1DataResponse);

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        EntityTransferMasterLists masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT_CODES.getId());
        masterData.setItemValue(hawbModel.getAwb().getAwbCargoInfo().getChargeCode());
        masterData.setItemDescription("IND");
        masterData.setIdentifier1("true");
        masterData.setIdentifier2("true");
        masterData.setIdentifier3("true");
        masterData.setIdentifier4("true");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.FREIGHT_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.OTHER_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class)).thenReturn(masterDataList);

        AwbGoodsDescriptionInfoModel awbGoodsDescriptionInfoModel = new AwbGoodsDescriptionInfoModel();
        List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModelList = new ArrayList<>();
        awbGoodsDescriptionInfoModelList.add(awbGoodsDescriptionInfoModel);
        when(modelMapper.map(any(), eq(AwbGoodsDescriptionInfoModel.class))).thenReturn(awbGoodsDescriptionInfoModel);

        EntityTransferCarrier entityTransferCarrier = new EntityTransferCarrier();
        entityTransferCarrier.setIATACode("123");
        entityTransferCarrier.setItemDescription("123");
        entityTransferCarrier.setItemValue("Turkish Airlines");
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferCarrier());

        Map<String,Object> packingValueMap = new HashMap<>();
        packingValueMap.put(ReportConstants.RATE_CLASS, 1);
        packingValueMap.put(ReportConstants.GROSS_WT, 0);
        packingValueMap.put(ReportConstants.CHARGEABLE_WT, 0);
        packingValueMap.put(ReportConstants.RATE_CHARGE, 0);
        packingValueMap.put(ReportConstants.TOTAL_AMOUNT, 0);
        packingValueMap.put(ReportConstants.PIECES_NO, 0);
        packingValueMap.put(ReportConstants.GROSS_VOLUME, 0);
        packingValueMap.put(GROSS_VOLUME_UNIT, "M3");
        packingValueMap.put(DIMENSIONS, "DIMS");
        packingValueMap.put(NATURE_OF_GOODS, "NatureOfGoods");
        packingValueMap.put(HS_CODE1, 123456);
        packingValueMap.put(SLAC_CODE, 654321);

        List<Map<String,Object>> dataMap = Arrays.asList(new HashMap<>(packingValueMap));
        doReturn(dataMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        doReturn(Arrays.asList(new HashMap<>(packingValueMap))).when(jsonHelper).convertValue(eq(dataMap), any(TypeReference.class));

        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(new OrgAddressResponse());

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(new CarrierMasterData())).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(new CarrierMasterData()));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferOrganizations());
        mockTenantSettings();
        UserContext.getUser().setEnableTimeZone(false);
        UserContext.getUser().setTimeZoneId("12");
        Map<String, Object> dict = hawbReport.populateDictionary(hawbModel);
        assertNotNull(dict);
        assertNotNull(dict.get(ReportConstants.ORDER_MANAGEMENT_NUMBER));
        assertEquals("1234-5678-9123-4567,1235-5678-9123-4567,1235-5679-9123-4567", dict.get(ReportConstants.ORDER_MANAGEMENT_NUMBER));
    }

    @Test
    void populateDictionaryWithCompanyDetails() {
        HawbModel hawbModel = new HawbModel();
        hawbModel.setEntityType(HAWB);

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.AIR);
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

        List<ReferenceNumbersModel>  referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(CEN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(FRN);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

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
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setShippingLine("AIR lINE");
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("AIR");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hawbModel.setShipmentDetails(shipmentModel);
        String csdInfo = "some info";
        hawb.getAwbCargoInfo().setCsdInfo(csdInfo);
        hawb.setOriginalPrintedAt(LocalDateTime.now());
        hawb.getAwbCargoInfo().setOtherInfoCode("test");
        AwbOtherInfo otherInfo = hawb.getAwbOtherInfo();
        otherInfo.setCarrierName("carrierName");
        otherInfo.setCarrierHqAddress("hqAddress");
        otherInfo.setLegalCompanyName("legal");
        otherInfo.setAddress1("address1");
        otherInfo.setAddress2("address2");
        otherInfo.setState("state");
        otherInfo.setCity("city");
        otherInfo.setCountryCode("IN");
        otherInfo.setCountryName("India");
        otherInfo.setBranch("branch");
        hawb.setAwbOtherInfo(otherInfo);
        hawbModel.setAwb(hawb);
        UsersDto usersDto = new UsersDto();
        usersDto.setUsername("UserName");
        usersDto.setCompanyId(1);
        hawbModel.setUsersDto(usersDto);
        hawbModel.getAwb().setAwbNotifyPartyInfo(List.of(AwbNotifyPartyInfo.builder().name("Hello").address("test address").build()));
        hawbModel.getAwb().getAwbShipmentInfo().setAccountNumber("123");
        hawbModel.getAwb().getAwbShipmentInfo().setShipperAccountNumber("456");
        hawbModel.getAwb().getAwbShipmentInfo().setConsigneeAccountNumber("789");

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        TenantModel tenantModel = new TenantModel();
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        v1TenantSettingsResponse.setLegalEntityCode("EntityCode");
        List<CompanyDto> companyDtoList = new ArrayList<>();
        companyDtoList.add(new CompanyDto());
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtoList);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1DataResponse);
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        EntityTransferMasterLists masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT_CODES.getId());
        masterData.setItemValue(hawbModel.getAwb().getAwbCargoInfo().getChargeCode());
        masterData.setItemDescription("IND");
        masterData.setIdentifier1("true");
        masterData.setIdentifier2("true");
        masterData.setIdentifier3("true");
        masterData.setIdentifier4("true");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.FREIGHT_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.OTHER_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class)).thenReturn(masterDataList);

        AwbGoodsDescriptionInfoModel awbGoodsDescriptionInfoModel = new AwbGoodsDescriptionInfoModel();
        List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModelList = new ArrayList<>();
        awbGoodsDescriptionInfoModelList.add(awbGoodsDescriptionInfoModel);
        when(modelMapper.map(any(), eq(AwbGoodsDescriptionInfoModel.class))).thenReturn(awbGoodsDescriptionInfoModel);

        EntityTransferCarrier entityTransferCarrier = new EntityTransferCarrier();
        entityTransferCarrier.setIATACode("123");
        entityTransferCarrier.setItemDescription("123");
        entityTransferCarrier.setItemValue("Turkish Airlines");
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferCarrier());
        Map<String,Object> packingValueMap = new HashMap<>();
        packingValueMap.put(ReportConstants.RATE_CLASS, 1);
        packingValueMap.put(ReportConstants.GROSS_WT, 0);
        packingValueMap.put(ReportConstants.CHARGEABLE_WT, 0);
        packingValueMap.put(ReportConstants.RATE_CHARGE, 0);
        packingValueMap.put(ReportConstants.TOTAL_AMOUNT, 0);
        packingValueMap.put(ReportConstants.PIECES_NO, 0);
        packingValueMap.put(HS_CODE1, 123456);
        packingValueMap.put(SLAC_CODE, 654321);
        packingValueMap.put(ReportConstants.GROSS_VOLUME, 0);
        packingValueMap.put(GROSS_VOLUME_UNIT, "M3");
        packingValueMap.put(DIMENSIONS, "DIMS");
        packingValueMap.put(NATURE_OF_GOODS, "NatureOfGoods");

        List<Map<String,Object>> dataMap = Arrays.asList(new HashMap<>(packingValueMap));
        doReturn(dataMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        doReturn(Arrays.asList(new HashMap<>(packingValueMap))).when(jsonHelper).convertValue(eq(dataMap), any(TypeReference.class));

        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(new OrgAddressResponse());

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(new CarrierMasterData())).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(new CarrierMasterData()));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferOrganizations());
        mockTenantSettings();
        UserContext.getUser().setEnableTimeZone(false);
        UserContext.getUser().setTimeZoneId("12");
        assertNotNull(hawbReport.populateDictionary(hawbModel));
    }

    @Test
    void populateDictionary3() {
        HawbModel hawbModel = new HawbModel();
        hawbModel.setEntityType(HAWB);

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.AIR);
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

        List<ReferenceNumbersModel>  referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(CEN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(FRN);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

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
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setShippingLine("AIR lINE");
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("AIR");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hawbModel.setShipmentDetails(shipmentModel);
        String csdInfo = "some info";
        hawb.getAwbCargoInfo().setCsdInfo(csdInfo);
        hawb.setOriginalPrintedAt(LocalDateTime.now());
        hawb.getAwbCargoInfo().setOtherInfoCode("test");
        AwbOtherInfo otherInfo = hawb.getAwbOtherInfo();
        otherInfo.setCarrierName("carrierName");
        otherInfo.setCarrierHqAddress("hqAddress");
        otherInfo.setLegalCompanyName("legal");
        otherInfo.setAddress1("address1");
        otherInfo.setAddress2("address2");
        otherInfo.setState("state");
        otherInfo.setCity("city");
        otherInfo.setCountryCode("IN");
        otherInfo.setCountryName("India");
        otherInfo.setBranch("branch");
        hawb.setAwbOtherInfo(otherInfo);
        hawbModel.setAwb(hawb);
        hawbModel.getAwb().setAwbNotifyPartyInfo(List.of(AwbNotifyPartyInfo.builder().name(null).build()));
        UsersDto usersDto = new UsersDto();
        usersDto.setUsername("UserName");
        usersDto.setCompanyId(1);
        hawbModel.setUsersDto(usersDto);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        TenantModel tenantModel = new TenantModel();
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        v1TenantSettingsResponse.setLegalEntityCode("EntityCode");
        CompanyDto companyDto = new CompanyDto();
        companyDto.setId(12);
        companyDto.setCountry("India");
        companyDto.setCompanyName("CompanyName");
        companyDto.setCity("city");
        companyDto.setCode("code");
        companyDto.setAddress1("Address1");
        companyDto.setAddress2("Address2");
        companyDto.setZipPostCode("ZipCode");
        List<CompanyDto> companyDtoList = new ArrayList<>();
        companyDtoList.add(companyDto);
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtoList);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1DataResponse);

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        EntityTransferMasterLists masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT_CODES.getId());
        masterData.setItemValue(hawbModel.getAwb().getAwbCargoInfo().getChargeCode());
        masterData.setItemDescription("IND");
        masterData.setIdentifier1("true");
        masterData.setIdentifier2("true");
        masterData.setIdentifier3("true");
        masterData.setIdentifier4("true");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.FREIGHT_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.OTHER_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class)).thenReturn(masterDataList);

        AwbGoodsDescriptionInfoModel awbGoodsDescriptionInfoModel = new AwbGoodsDescriptionInfoModel();
        List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModelList = new ArrayList<>();
        awbGoodsDescriptionInfoModelList.add(awbGoodsDescriptionInfoModel);
        when(modelMapper.map(any(), eq(AwbGoodsDescriptionInfoModel.class))).thenReturn(awbGoodsDescriptionInfoModel);

        EntityTransferCarrier entityTransferCarrier = new EntityTransferCarrier();
        entityTransferCarrier.setIATACode("123");
        entityTransferCarrier.setItemDescription("123");
        entityTransferCarrier.setItemValue("Turkish Airlines");
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferCarrier());

        Map<String,Object> packingValueMap = new HashMap<>();
        packingValueMap.put(ReportConstants.RATE_CLASS, 1);
        packingValueMap.put(ReportConstants.GROSS_WT, 0);
        packingValueMap.put(ReportConstants.CHARGEABLE_WT, 0);
        packingValueMap.put(ReportConstants.RATE_CHARGE, 0);
        packingValueMap.put(ReportConstants.TOTAL_AMOUNT, 0);
        packingValueMap.put(ReportConstants.PIECES_NO, 0);
        packingValueMap.put(ReportConstants.GROSS_VOLUME, 0);
        packingValueMap.put(GROSS_VOLUME_UNIT, "M3");
        packingValueMap.put(DIMENSIONS, "DIMS");
        packingValueMap.put(NATURE_OF_GOODS, "NatureOfGoods");
        packingValueMap.put(HS_CODE1, 123456);
        packingValueMap.put(SLAC_CODE, 654321);

        List<Map<String,Object>> dataMap = Arrays.asList(new HashMap<>(packingValueMap));
        doReturn(dataMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        doReturn(Arrays.asList(new HashMap<>(packingValueMap))).when(jsonHelper).convertValue(eq(dataMap), any(TypeReference.class));

        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(new OrgAddressResponse());

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(new CarrierMasterData())).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(new CarrierMasterData()));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferOrganizations());
        mockTenantSettings();
        UserContext.getUser().setEnableTimeZone(false);
        UserContext.getUser().setTimeZoneId("12");

        Map<String, Object> dictionary = hawbReport.populateDictionary(hawbModel);

        assertEquals("", dictionary.get(AWB_NOTIFY_PARTY_NAME));
        assertNotNull(dictionary);
    }

    @Test
    void populateDictionary() {
        HawbModel hawbModel = new HawbModel();
        hawbModel.setEntityType(HAWB);

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.AIR);
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

        List<ReferenceNumbersModel>  referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(CEN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(FRN);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

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
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setShippingLine("AIR lINE");
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("AIR");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hawbModel.setShipmentDetails(shipmentModel);
        hawbModel.setAwb(hawb);
        UsersDto usersDto = new UsersDto();
        usersDto.setUsername("UserName");
        usersDto.setCompanyId(1);
        hawbModel.setUsersDto(usersDto);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        TenantModel tenantModel = new TenantModel();

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        v1TenantSettingsResponse.setLegalEntityCode("EntityCode");
        CompanyDto companyDto = new CompanyDto();
        companyDto.setId(12);
        companyDto.setCountry("India");
        companyDto.setCompanyName("CompanyName");
        companyDto.setCity("city");
        companyDto.setCode("code");
        companyDto.setAddress1("Address1");
        companyDto.setAddress2("Address2");
        companyDto.setZipPostCode("ZipCode");
        List<CompanyDto> companyDtoList = new ArrayList<>();
        companyDtoList.add(companyDto);
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtoList);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1DataResponse);

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        EntityTransferMasterLists masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT_CODES.getId());
        masterData.setItemValue(hawbModel.getAwb().getAwbCargoInfo().getChargeCode());
        masterData.setItemDescription("IND");
        masterData.setIdentifier1("true");
        masterData.setIdentifier2("true");
        masterData.setIdentifier3("true");
        masterData.setIdentifier4("true");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.FREIGHT_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.MAWB_CHARGE_TEXT.getId());
        masterData.setItemValue(AwbConstants.OTHER_AMOUNT);
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class)).thenReturn(masterDataList);

        AwbGoodsDescriptionInfoModel awbGoodsDescriptionInfoModel = new AwbGoodsDescriptionInfoModel();
        List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModelList = new ArrayList<>();
        awbGoodsDescriptionInfoModelList.add(awbGoodsDescriptionInfoModel);
        when(modelMapper.map(any(), eq(AwbGoodsDescriptionInfoModel.class))).thenReturn(awbGoodsDescriptionInfoModel);

        EntityTransferCarrier entityTransferCarrier = new EntityTransferCarrier();
        entityTransferCarrier.setIATACode("123");
        entityTransferCarrier.setItemDescription("123");
        entityTransferCarrier.setItemValue("Turkish Airlines");
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferCarrier());

        Map<String,Object> packingValueMap = new HashMap<>();
        packingValueMap.put(ReportConstants.RATE_CLASS, 1);
        packingValueMap.put(ReportConstants.GROSS_WT, 0);
        packingValueMap.put(ReportConstants.CHARGEABLE_WT, 0);
        packingValueMap.put(ReportConstants.RATE_CHARGE, 0);
        packingValueMap.put(ReportConstants.TOTAL_AMOUNT, 0);
        packingValueMap.put(ReportConstants.PIECES_NO, 0);
        packingValueMap.put(ReportConstants.GROSS_VOLUME, 0);
        packingValueMap.put(GROSS_VOLUME_UNIT, "M3");
        packingValueMap.put(DIMENSIONS, "DIMS");
        packingValueMap.put(NATURE_OF_GOODS, "NatureOfGoods");
        packingValueMap.put(HS_CODE1, 123456);
        packingValueMap.put(SLAC_CODE, 654321);

        List<Map<String,Object>> dataMap = Arrays.asList(new HashMap<>(packingValueMap));
        doReturn(dataMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        doReturn(Arrays.asList(new HashMap<>(packingValueMap))).when(jsonHelper).convertValue(eq(dataMap), any(TypeReference.class));

        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(new OrgAddressResponse());

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(new CarrierMasterData())).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(new CarrierMasterData()));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferOrganizations());
        mockTenantSettings();
        UserContext.getUser().setEnableTimeZone(false);
        UserContext.getUser().setTimeZoneId("12");
        assertNotNull(hawbReport.populateDictionary(hawbModel));
    }

    @Test
    void populateDictionaryWithMAwb() {
        HawbModel hawbModel = new HawbModel();
        hawbModel.setEntityType(MAWB);

        List<ReferenceNumbersModel>  referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(CEN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(FRN);
        referenceNumbersModels.add(referenceNumbersModel);

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);


        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setShippingLine("AIR lINE");

        hawbModel.setAwb(hawb);
        hawbModel.setMawb(mawb);
        UsersDto usersDto = new UsersDto();
        usersDto.setUsername("UserName");
        usersDto.setCompanyId(1);
        hawbModel.setUsersDto(usersDto);

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
        hawbModel.setConsolidationDetails(consolidationModel);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        TenantModel tenantModel = new TenantModel();

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        v1TenantSettingsResponse.setLegalEntityCode("EntityCode");
        CompanyDto companyDto = new CompanyDto();
        companyDto.setId(12);
        companyDto.setCountry("India");
        companyDto.setCompanyName("CompanyName");
        companyDto.setCity("city");
        companyDto.setCode("code");
        companyDto.setAddress1("Address1");
        companyDto.setAddress2("Address2");
        companyDto.setZipPostCode("ZipCode");
        List<CompanyDto> companyDtoList = new ArrayList<>();
        companyDtoList.add(companyDto);
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtoList);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1DataResponse);

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        EntityTransferMasterLists masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT_CODES.getId());
        masterData.setItemValue(hawbModel.getAwb().getAwbCargoInfo().getChargeCode());
        masterData.setItemDescription("IND");
        masterData.setIdentifier1("true");
        masterData.setIdentifier2("true");
        masterData.setIdentifier3("true");
        masterData.setIdentifier4("true");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue(consolidationModel.getPayment());
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class)).thenReturn(masterDataList);

        AwbGoodsDescriptionInfoModel awbGoodsDescriptionInfoModel = new AwbGoodsDescriptionInfoModel();
        List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModelList = new ArrayList<>();
        awbGoodsDescriptionInfoModelList.add(awbGoodsDescriptionInfoModel);
        when(modelMapper.map(any(), eq(AwbGoodsDescriptionInfoModel.class))).thenReturn(awbGoodsDescriptionInfoModel);

        EntityTransferCarrier entityTransferCarrier = new EntityTransferCarrier();
        entityTransferCarrier.setIATACode("123");
        entityTransferCarrier.setItemDescription("123");
        entityTransferCarrier.setItemValue("Turkish Airlines");
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferCarrier());

        doReturn(new ArrayList<>()).when(jsonHelper).convertValue(any(), any(TypeReference.class));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferOrganizations());
        mockTenantSettings();
        UserContext.getUser().setEnableTimeZone(false);
        UserContext.getUser().setTimeZoneId("12");
        assertNotNull(hawbReport.populateDictionary(hawbModel));
    }

    @Test
    void populateDictionaryDMawb() {
        HawbModel hawbModel = new HawbModel();
        hawbModel.setEntityType(AwbConstants.DMAWB);

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.AIR);
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

        List<ReferenceNumbersModel>  referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(CEN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(FRN);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

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
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setShippingLine("AIR lINE");
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("RFS");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setCommodity("AIR");
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setEstimatedPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hawbModel.setShipmentDetails(shipmentModel);
        hawbModel.setAwb(hawb);
        UsersDto usersDto = new UsersDto();
        usersDto.setUsername("UserName");
        usersDto.setCompanyId(1);
        hawbModel.setUsersDto(usersDto);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        TenantModel tenantModel = new TenantModel();

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        v1TenantSettingsResponse.setLegalEntityCode("EntityCode");
        CompanyDto companyDto = new CompanyDto();
        companyDto.setId(12);
        companyDto.setCountry("India");
        companyDto.setCompanyName("CompanyName");
        companyDto.setCity("city");
        companyDto.setCode("code");
        companyDto.setAddress1("Address1");
        companyDto.setAddress2("Address2");
        companyDto.setZipPostCode("ZipCode");
        List<CompanyDto> companyDtoList = new ArrayList<>();
        companyDtoList.add(companyDto);
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtoList);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1DataResponse);

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<EntityTransferMasterLists> masterDataList = new ArrayList<>();
        EntityTransferMasterLists masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);

        masterData = new EntityTransferMasterLists();
        masterData.setItemType(MasterDataType.PAYMENT_CODES.getId());
        masterData.setItemValue(hawbModel.getAwb().getAwbCargoInfo().getChargeCode());
        masterData.setItemDescription("IND");
        masterData.setIdentifier1("true");
        masterData.setIdentifier2("true");
        masterData.setIdentifier3("true");
        masterData.setIdentifier4("true");
        masterDataList.add(masterData);

        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class)).thenReturn(masterDataList);

        AwbGoodsDescriptionInfoModel awbGoodsDescriptionInfoModel = new AwbGoodsDescriptionInfoModel();
        List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModelList = new ArrayList<>();
        awbGoodsDescriptionInfoModelList.add(awbGoodsDescriptionInfoModel);
        when(modelMapper.map(any(), eq(AwbGoodsDescriptionInfoModel.class))).thenReturn(awbGoodsDescriptionInfoModel);

        EntityTransferCarrier entityTransferCarrier = new EntityTransferCarrier();
        entityTransferCarrier.setIATACode("123");
        entityTransferCarrier.setItemDescription("123");
        entityTransferCarrier.setItemValue("Turkish Airlines");
        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferCarrier());
//        when(v1Service.fetchCarrierMasterData(any(), eq(true))).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferCarrier.class)).thenReturn(Arrays.asList(entityTransferCarrier));

        doReturn(new ArrayList<>()).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(new OrgAddressResponse());

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(new CarrierMasterData())).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(new CarrierMasterData()));

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new EntityTransferOrganizations());
        UserContext.getUser().setEnableTimeZone(false);
        UserContext.getUser().setTimeZoneId("12");
        mockTenantSettings();
        assertNotNull(hawbReport.populateDictionary(hawbModel));
    }

    @Test
    void getDocumentModel() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        PackingModel packingModel = new PackingModel();
        packingModel.setHazardous(true);
        shipmentModel.setPackingList(List.of(packingModel));
        shipmentModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(awbDao.findByConsolidationId(any())).thenReturn(Arrays.asList(new Awb()));
        mockShipmentSettings();
        assertNotNull(hawbReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_dgUserError() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        PackingModel packingModel = new PackingModel();
        packingModel.setHazardous(true);
        shipmentModel.setPackingList(List.of(packingModel));
        shipmentModel.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        UserContext.getUser().setPermissions(new HashMap<>());
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> hawbReport.getDocumentModel(123L));
    }
}
