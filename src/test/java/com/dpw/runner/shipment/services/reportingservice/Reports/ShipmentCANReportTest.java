package com.dpw.runner.shipment.services.reportingservice.Reports;

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.AIR;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CALCULATED_VALUE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CHARGE_TYPE_CODE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.COMPANY_NAME;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CONTACT_PERSON;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CURRENT_SELL_RATE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.CUSTOM_HOUSE_AGENT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.EXP;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.HAWB;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.KCRA_EXPIRY;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.KCRA_NUMBER;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.KNOWN_CONSIGNOR;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.MEASUREMENT_BASIS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.ONE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.OVERSEAS_SELL_AMOUNT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.OVERSEAS_TAX;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.PRE_CARRIAGE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.REGULATED_AGENT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.REVENUE_CALCULATED_VALUE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.REVENUE_MEASUREMENT_BASIS;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.SEA;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.SELL_EXCHANGE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.TAX_PERCENTAGE;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.TOTAL_AMOUNT;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.TWO;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentCANModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.AdditionalDetailModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.CarrierDetailModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbShipmentInfo;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
class ShipmentCANReportTest extends CommonMocks {

    @InjectMocks
    private ShipmentCANReport shipmentCANReport;

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
    private BillingServiceUrlConfig billingServiceUrlConfig;
    @Mock
    private BillingServiceAdapter billingServiceAdapter;

    @Mock
    private HblReport hblReport;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IAwbDao awbDao;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        mockUser.setCompanyCurrency("INR");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    private static ShipmentDetails shipmentDetails;

    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).GSTTaxAutoCalculation(true).CurrencyDigitGrouping(1).CurrencyGroupingNumber(GroupingNumber.DOT_AND_COMMA.getValue()).IsGroupingOverseas(false).RoundoffLocalCurrencyAmount(true).build());
    }

    @Test
    void populateDictionary_BillingIntegrationDisabled() {
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();

        shipmentCANModel.tenantDetails =  new TenantModel();
        shipmentCANModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentCANModel.tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();

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

        PartiesModel partiesModel2 = new PartiesModel();
        partiesModel2.setOrgCode("Test2");
        partiesModel2.setAddressCode("Test2");

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel2);
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

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        shipmentCANModel.shipmentDetails = shipmentModel;

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
        shipmentCANModel.consolidationModel = consolidationModel;

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(hblReport.getData(any())).thenReturn(new HashMap<>());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        List<BillingResponse> billingResponseList = Arrays.asList(new BillingResponse());
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(billingResponseList).build();
        when(v1MasterData.fetchBillingList(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class)).thenReturn(billingResponseList);

        BillChargesResponse billChargesResponse = new BillChargesResponse();
        List<BillChargesResponse> billChargesResponseList = Arrays.asList(billChargesResponse);
        dependentServiceResponse = DependentServiceResponse.builder().data(billChargesResponseList).build();
        when(v1MasterData.fetchBillChargesList(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class)).thenReturn(billChargesResponseList);

        dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

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

        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);

        Map<String, Object> billChargeMap = new HashMap<>();
        billChargeMap.put(REVENUE_MEASUREMENT_BASIS, MeasurementBasis.CHARGEABLE.getValue());
        billChargeMap.put(REVENUE_CALCULATED_VALUE, "1 2");
        billChargeMap.put(MEASUREMENT_BASIS, MeasurementBasis.CHARGEABLE.getValue());
        billChargeMap.put(CALCULATED_VALUE, "1 2");
        billChargeMap.put(SELL_EXCHANGE, BigDecimal.TEN);
        billChargeMap.put(CURRENT_SELL_RATE, BigDecimal.TEN);
        billChargeMap.put(OVERSEAS_SELL_AMOUNT, BigDecimal.TEN);
        billChargeMap.put(OVERSEAS_TAX, BigDecimal.TEN);
        billChargeMap.put(TAX_PERCENTAGE, BigDecimal.TEN);
        billChargeMap.put(TOTAL_AMOUNT, BigDecimal.TEN);
        billChargeMap.put(CHARGE_TYPE_CODE, "20GP");
        doReturn(billChargeMap).when(jsonHelper).convertValue(eq(billChargesResponse), any(TypeReference.class));
        mockTenantSettings();
        assertNotNull(shipmentCANReport.populateDictionary(shipmentCANModel));
    }

    @Test
    void populateDictionary_BillingIntegrationEnabled() {
        UUID randomShipmentGuid = UUID.randomUUID();
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();

        shipmentCANModel.tenantDetails =  new TenantModel();
        shipmentCANModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentCANModel.tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();

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

        PartiesModel partiesModel2 = new PartiesModel();
        partiesModel2.setOrgCode("Test2");
        partiesModel2.setAddressCode("Test2");

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel2);
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
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        additionalDetailModel.setExportBroker(partiesModel);
        additionalDetailModel.setImportBroker(partiesModel);
        additionalDetailModel.setScreeningStatus(List.of(Constants.AOM));
        additionalDetailModel.setExemptionCodes("Test");
        additionalDetailModel.setAomFreeText("Test");
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(List.of(partiesModel));

        PickupDeliveryDetailsModel deliveryDetailsModel = new PickupDeliveryDetailsModel();
        deliveryDetailsModel.setActualPickupOrDelivery(LocalDateTime.now());
        deliveryDetailsModel.setDestinationDetail(partiesModel);
        deliveryDetailsModel.setAgentDetail(partiesModel);
        deliveryDetailsModel.setSourceDetail(partiesModel);
        deliveryDetailsModel.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(deliveryDetailsModel);
        shipmentModel.setDeliveryDetails(deliveryDetailsModel);
        shipmentCANModel.shipmentDetails = shipmentModel;
        shipmentCANModel.shipmentDetails.setGuid(randomShipmentGuid);


        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(List.of(bookingCarriageModel));

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
        shipmentCANModel.consolidationModel = consolidationModel;

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);

        BillBaseResponse billFromBilling = new BillBaseResponse();
        billFromBilling.setGuId(UUID.randomUUID().toString());
        billFromBilling.setBillId("BIL123");
        billFromBilling.setRemarks("");

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(billingServiceAdapter.fetchBill(any())).thenReturn(billFromBilling);
        when(hblReport.getData(any())).thenReturn(new HashMap<>());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

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

        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);

        mockTenantSettings();
        assertNotNull(shipmentCANReport.populateDictionary(shipmentCANModel));
    }

    @Test
    void populateDictionaryWithoutV2Billing_BillingIntegrationDisabled() {
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();

        shipmentCANModel.tenantDetails =  new TenantModel();
        shipmentCANModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentCANModel.tenantSettingsResponse =  V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(false).GSTTaxAutoCalculation(true).build();

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("USD");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("USD");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setNoOfPacks(10);
        shipmentModel.setSecurityStatus("Test");

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

        PartiesModel partiesModel2 = new PartiesModel();
        partiesModel2.setOrgCode("Test2");
        partiesModel2.setAddressCode("Test2");

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel2);
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

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        shipmentCANModel.shipmentDetails = shipmentModel;

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
        shipmentCANModel.consolidationModel = consolidationModel;

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(hblReport.getData(any())).thenReturn(new HashMap<>());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);

        List<BillingResponse> billingResponseList = Arrays.asList(new BillingResponse());
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(billingResponseList).build();
        when(v1MasterData.fetchBillingList(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class)).thenReturn(billingResponseList);

        BillChargesResponse billChargesResponse = new BillChargesResponse();
        List<BillChargesResponse> billChargesResponseList = Arrays.asList(billChargesResponse);
        dependentServiceResponse = DependentServiceResponse.builder().data(billChargesResponseList).build();
        when(v1MasterData.fetchBillChargesList(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class)).thenReturn(billChargesResponseList);

        dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

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

        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);

        Map<String, Object> billChargeMap = new HashMap<>();
        billChargeMap.put(REVENUE_MEASUREMENT_BASIS, MeasurementBasis.CHARGEABLE.getValue());
        billChargeMap.put(REVENUE_CALCULATED_VALUE, "1 2");
        billChargeMap.put(MEASUREMENT_BASIS, MeasurementBasis.CHARGEABLE.getValue());
        billChargeMap.put(CALCULATED_VALUE, "1 2");
        billChargeMap.put(SELL_EXCHANGE, BigDecimal.TEN);
        billChargeMap.put(CURRENT_SELL_RATE, BigDecimal.TEN);
        billChargeMap.put(OVERSEAS_SELL_AMOUNT, BigDecimal.TEN);
        billChargeMap.put(OVERSEAS_TAX, BigDecimal.TEN);
        billChargeMap.put(TAX_PERCENTAGE, BigDecimal.TEN);
        billChargeMap.put(TOTAL_AMOUNT, BigDecimal.TEN);
        billChargeMap.put(CHARGE_TYPE_CODE, "20GP");

        doReturn(billChargeMap).when(jsonHelper).convertValue(eq(billChargesResponse), any(TypeReference.class));
        mockTenantSettings();
        shipmentCANReport.populateDictionary(shipmentCANModel);
        assertNotNull(shipmentCANReport.populateDictionary(shipmentCANModel));
    }

    @Test
    void populateDictionaryWithoutV2Billing_BillingIntegrationEnabled() {
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();

        shipmentCANModel.tenantDetails =  new TenantModel();
        shipmentCANModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentCANModel.tenantSettingsResponse =  V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(false).GSTTaxAutoCalculation(true).build();

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("USD");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("USD");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setNoOfPacks(10);
        shipmentModel.setSecurityStatus("Test");

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

        PartiesModel partiesModel2 = new PartiesModel();
        partiesModel2.setOrgCode("Test2");
        partiesModel2.setAddressCode("Test2");

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel2);
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

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        shipmentCANModel.shipmentDetails = shipmentModel;
        shipmentCANModel.shipmentDetails.setGuid(UUID.randomUUID());

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
        shipmentCANModel.consolidationModel = consolidationModel;

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);

        BillBaseResponse billFromBilling = new BillBaseResponse();
        billFromBilling.setGuId(UUID.randomUUID().toString());
        billFromBilling.setBillId("BIL123");
        billFromBilling.setRemarks("");

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(billingServiceAdapter.fetchBill(any())).thenReturn(billFromBilling);
        when(hblReport.getData(any())).thenReturn(new HashMap<>());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);

        List<BillingResponse> billingResponseList = Arrays.asList(new BillingResponse());
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(billingResponseList).build();
//        when(v1MasterData.fetchBillingList(any())).thenReturn(dependentServiceResponse);
//        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class)).thenReturn(billingResponseList);

        BillChargesResponse billChargesResponse = new BillChargesResponse();
        List<BillChargesResponse> billChargesResponseList = Arrays.asList(billChargesResponse);
        dependentServiceResponse = DependentServiceResponse.builder().data(billChargesResponseList).build();
//        when(v1MasterData.fetchBillChargesList(any())).thenReturn(dependentServiceResponse);
//        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class)).thenReturn(billChargesResponseList);

        dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

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

        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);

        Map<String, Object> billChargeMap = new HashMap<>();
        billChargeMap.put(REVENUE_MEASUREMENT_BASIS, MeasurementBasis.CHARGEABLE.getValue());
        billChargeMap.put(REVENUE_CALCULATED_VALUE, "1 2");
        billChargeMap.put(MEASUREMENT_BASIS, MeasurementBasis.CHARGEABLE.getValue());
        billChargeMap.put(CALCULATED_VALUE, "1 2");
        billChargeMap.put(SELL_EXCHANGE, BigDecimal.TEN);
        billChargeMap.put(CURRENT_SELL_RATE, BigDecimal.TEN);
        billChargeMap.put(OVERSEAS_SELL_AMOUNT, BigDecimal.TEN);
        billChargeMap.put(OVERSEAS_TAX, BigDecimal.TEN);
        billChargeMap.put(TAX_PERCENTAGE, BigDecimal.TEN);
        billChargeMap.put(TOTAL_AMOUNT, BigDecimal.TEN);
        billChargeMap.put(CHARGE_TYPE_CODE, "20GP");

//        doReturn(billChargeMap).when(jsonHelper).convertValue(eq(billChargesResponse), any(TypeReference.class));
        mockTenantSettings();
        shipmentCANReport.populateDictionary(shipmentCANModel);
        assertNotNull(shipmentCANReport.populateDictionary(shipmentCANModel));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        when(shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(1))).thenReturn(Arrays.asList(ShipmentSettingsDetails.builder().build()));
        mockTenantSettings();
        assertNotNull(shipmentCANReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModelAir() throws RunnerException {
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setDirection(EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        when(shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(1))).thenReturn(Arrays.asList(ShipmentSettingsDetails.builder().build()));

        Awb awb = new Awb();
        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType(HAWB).build());
//        when(awbDao.findByShipmentIdList(any())).thenReturn(Arrays.asList(awb));
        mockTenantSettings();
        assertNotNull(shipmentCANReport.getDocumentModel(123L));
        assert (true);
    }
}
