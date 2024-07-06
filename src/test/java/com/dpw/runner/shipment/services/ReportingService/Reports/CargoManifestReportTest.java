package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(MockitoExtension.class)
class CargoManifestReportTest extends CommonMocks {
    @InjectMocks
    private CargoManifestReport cargoManifestReport;

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
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private HblReport hblReport;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IAwbRepository awbRepository;

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
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).GSTTaxAutoCalculation(true).build());
    }

    @Test
    void populateDictionary() {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();

        cargoManifestModel.tenantDetails =  new TenantModel();
        cargoManifestModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        cargoManifestModel.usersDto = UserContext.getUser();
        cargoManifestModel.awb = new Awb();
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        awbCargoInfo.setSci("test");
        cargoManifestModel.awb.setAwbCargoInfo(awbCargoInfo);

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
        shipmentModel.setSecurityStatus("Test");
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setPacksUnit("PKG");

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setOrgCode("Test");
        partiesModel.setAddressCode("Test");
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
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
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));

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
        shipmentModel.setContainersList(containerModelList);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        cargoManifestModel.shipmentDetails = shipmentModel;

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);

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

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TareWeight, BigDecimal.TEN);
        containerMap.put(VGMWeight, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));
        mockTenantSettings();
        assertNotNull(cargoManifestReport.populateDictionary(cargoManifestModel));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setDirection(EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        mockShipmentSettings();
        assertNotNull(cargoManifestReport.getDocumentModel(123L));
    }
}
