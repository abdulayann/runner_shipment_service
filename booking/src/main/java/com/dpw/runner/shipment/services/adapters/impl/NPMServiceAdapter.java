package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.constants.TimeZoneConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchMultiLangChargeCodeRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.masterDataObjects.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.exception.exceptions.NPMException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.response.NpmErrorResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.math.BigDecimal;
import java.net.URI;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Service
@Slf4j
public class NPMServiceAdapter implements INPMServiceAdapter {

    public static final String PAYLOAD_SENT_FOR_EVENT_WITH_REQUEST_PAYLOAD_MSG = "Payload sent for event: {} with request payload: {}";
    public static final String NPM_FETCH_CONTRACT_FAILED_DUE_TO_MSG = "NPM Fetch contract failed due to: {}";
    public static final String ERROR_FROM_NPM_WHILE_FETCHING_CONTRACTS_MSG = "Error from NPM while fetching contracts: ";
    public static final String LOCATIONS_REFERENCE_GUID = "LocationsReferenceGUID";
    @Value("${NPM.BaseUrl}")
    private String npmBaseUrl;

    @Value("${npmservice.url.base}")
    private String npmServiceBaseUrl;

    @Value("${NPM.Contracts}")
    private String npmContracts;

    @Value("${NPM.Offers}")
    private String npmOffersUrl;

    @Value("${NPM.OffersV8}")
    private String npmOffersV8Url;


    @Value("${NPM.Update}")
    private String npmUpdateUrl;

    @Value("${npmservice.url.autosell}")
    private String npmAwbAutoSell;

    @Value("${npmservice.url.importrates}")
    private String npmAwbImportRates;
    @Value("${NPM.FetchMultiLangChargeCode}")
    private String npmMultiLangChargeCode;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    ModelMapper modelMapper;

    @Value("${NPM.xApikeyV2}")
    private String xApikeyV2;

    @Value("${npm.exchange.rate.url}")
    private String exchangeRateUrl;

    private final RestTemplate restTemplate;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    @Qualifier("restTemplateForNpmService")
    private RestTemplate npmServiceRestTemplate;
    @Autowired
    @Qualifier("restTemplateForNpmMultiLangChargeCode")
    private RestTemplate restTemplateMultiLang;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    public NPMServiceAdapter(@Qualifier("restTemplateForNPM") RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }
    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Autowired
    private IQuoteContractsService quoteContractsService;

    @Override
    public ResponseEntity<IRunnerResponse> updateContracts(CommonRequestModel commonRequestModel) throws RunnerException {
        try {
            UpdateContractRequest updateContractRequest = (UpdateContractRequest) commonRequestModel.getData();
            String url = npmBaseUrl + npmUpdateUrl;
            log.info(PAYLOAD_SENT_FOR_EVENT_WITH_REQUEST_PAYLOAD_MSG, IntegrationType.NPM_UPDATE_UTILISATION, jsonHelper.convertToJson(updateContractRequest));
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.patch(URI.create(url)).body(jsonHelper.convertToJson(updateContractRequest)), Object.class);
            return ResponseHelper.buildDependentServiceResponse(response.getBody(),0,0);
        } catch (HttpStatusCodeException ex) {
            NpmErrorResponse npmErrorResponse = jsonHelper.readFromJson(ex.getResponseBodyAsString(), NpmErrorResponse.class);
            log.error("NPM Update contract failed due to: {}", jsonHelper.convertToJson(npmErrorResponse));
            throw new NPMException("Error from NPM while updating utilisation: " + npmErrorResponse.getErrorMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchOffers(CommonRequestModel req) throws RunnerException {
        String url = npmBaseUrl + npmOffersUrl;
        NPMFetchOffersRequestFromUI fetchOffersRequest = (NPMFetchOffersRequestFromUI) req.getData();
        var request = createNPMOffersRequest(fetchOffersRequest);
        try {
            log.info(PAYLOAD_SENT_FOR_EVENT_WITH_REQUEST_PAYLOAD_MSG, IntegrationType.NPM_OFFER_FETCH_V2, jsonHelper.convertToJson(request));
            ResponseEntity<FetchOffersResponse> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), FetchOffersResponse.class);
            this.setMeasurementBasis(response.getBody());
            this.modifyOffersAPIData(response.getBody());
            return ResponseHelper.buildDependentServiceResponse(response.getBody(),0,0);
        } catch (HttpStatusCodeException ex) {
            NpmErrorResponse npmErrorResponse = jsonHelper.readFromJson(ex.getResponseBodyAsString(), NpmErrorResponse.class);
            log.error("NPM fetch offer failed due to: {}", jsonHelper.convertToJson(npmErrorResponse));
            throw new NPMException("Error from NPM while fetching offers: " + npmErrorResponse.getErrorMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchOffersV8(CommonRequestModel req) throws RunnerException {
        try {
            String url = npmBaseUrl + npmOffersV8Url;
            NPMFetchOffersRequestFromUI fetchOffersRequest = (NPMFetchOffersRequestFromUI) req.getData();
            var request = createNPMOffersV8Request(fetchOffersRequest);
            log.info(PAYLOAD_SENT_FOR_EVENT_WITH_REQUEST_PAYLOAD_MSG, IntegrationType.NPM_OFFER_FETCH_V8, jsonHelper.convertToJson(request));
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), Object.class);
            return ResponseHelper.buildDependentServiceResponse(response.getBody(),0,0);
        } catch (HttpStatusCodeException ex) {
            NpmErrorResponse npmErrorResponse = jsonHelper.readFromJson(ex.getResponseBodyAsString(), NpmErrorResponse.class);
            log.error("NPM fetch offers v8/offers failed due to: {}", jsonHelper.convertToJson(npmErrorResponse));
            throw new NPMException("Error from NPM while fetching offers: " + npmErrorResponse.getErrorMessage());
        }

    }


    private String getCurrencyCode()  {
        return UserContext.getUser().CompanyCurrency;
    }


    private String mapMeasurementBasis(String uom)
    {
        if(uom == null || uom.isEmpty())
            return uom;
        uom = uom.toLowerCase();
        switch(uom)
        {
            case "perctr":
                return "ContainerCount";
            case "percbm":
                return "Volume";
            case "perkg":
                return "Weight";
            case "pership":
                return "Shipment";
            default:
                return uom;
        }
    }

    private void modifyOffersAPIData(FetchOffersResponse response) {
        if(Objects.isNull(response) || Objects.isNull(response.getOffers()) || response.getOffers().isEmpty())
            return;
        FetchOffersResponse.Offer offer = response.getOffers().get(0);
        if(!IsStringNullOrEmpty(offer.getCarrier())) {
            List<String> carrierCodes = new ArrayList<>();
            carrierCodes.add(offer.getCarrier());
            Map<String, CarrierMasterData> map = masterDataUtils.fetchInBulkCarriersBySCACCode(carrierCodes);
            if(map.containsKey(offer.getCarrier()))
                offer.setCarrier(map.get(offer.getCarrier()).ItemValue);
            else
                offer.setCarrier(null);
        }
    }

    private void setMeasurementBasis(FetchOffersResponse response)
    {
        if(response != null && response.getOffers() != null && !response.getOffers().isEmpty())
        {
            FetchOffersResponse.Offer offer = response.getOffers().get(0);
            if(offer.getEntity_rate_cards() != null && !offer.getEntity_rate_cards().isEmpty())
            {
                for (FetchOffersResponse.EntityRateCard entityRateCard: offer.getEntity_rate_cards()) {
                    if(entityRateCard.getLoads_rates_info() != null && !entityRateCard.getLoads_rates_info().isEmpty())
                    {
                        for(FetchOffersResponse.LoadsRatesInfo loadsRatesInfo : entityRateCard.getLoads_rates_info())
                        {
                            if(loadsRatesInfo.getAssociated_rates() != null && !loadsRatesInfo.getAssociated_rates().isEmpty())
                            {
                                for(FetchOffersResponse.AssociatedRate associatedRate: loadsRatesInfo.getAssociated_rates())
                                {
                                    if(associatedRate != null)
                                    {
                                        associatedRate.setRates_uom(mapMeasurementBasis(associatedRate.getRates_uom()));
                                        if(Objects.equals(associatedRate.getRates_uom(), "ContainerCount"))
                                        {
                                            if(loadsRatesInfo.getQuantity() != null)
                                            {
                                                associatedRate.setTotal_unit_count(BigDecimal.valueOf(loadsRatesInfo.getQuantity()));
                                                associatedRate.setMeasurement_unit("Containers");
                                            }
                                        }
                                        else if(Objects.equals(associatedRate.getRates_uom(), "Shipment"))
                                        {
                                            associatedRate.setTotal_unit_count(BigDecimal.ONE);
                                            associatedRate.setMeasurement_unit("SHIPMENT");
                                        }
                                        else
                                        {
                                            associatedRate.setTotal_unit_count(associatedRate.getChargeable());
                                            associatedRate.setMeasurement_unit(associatedRate.getChargeable_uom());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if(entityRateCard.getAggregated_shipment_load_rates_info() != null && !entityRateCard.getAggregated_shipment_load_rates_info().isEmpty())
                    {
                        for(FetchOffersResponse.LoadsRatesInfo loadsRatesInfo : entityRateCard.getAggregated_shipment_load_rates_info())
                        {
                            if(loadsRatesInfo.getAssociated_rates() != null && !loadsRatesInfo.getAssociated_rates().isEmpty())
                            {
                                for(FetchOffersResponse.AssociatedRate associatedRate: loadsRatesInfo.getAssociated_rates())
                                {
                                    if(associatedRate != null) {
                                        associatedRate.setRates_uom(mapMeasurementBasis(associatedRate.getRates_uom()));
                                        if(Objects.equals(associatedRate.getRates_uom(), "ContainerCount"))
                                        {
                                            if(loadsRatesInfo.getQuantity() != null)
                                            {
                                                associatedRate.setTotal_unit_count(BigDecimal.valueOf(loadsRatesInfo.getQuantity()));
                                                associatedRate.setMeasurement_unit("Containers");
                                            }
                                        }
                                        else if(Objects.equals(associatedRate.getRates_uom(), "Shipment"))
                                        {
                                            associatedRate.setTotal_unit_count(BigDecimal.ONE);
                                            associatedRate.setMeasurement_unit("SHIPMENT");
                                        }
                                        else
                                        {
                                            associatedRate.setTotal_unit_count(associatedRate.getChargeable());
                                            associatedRate.setMeasurement_unit(associatedRate.getChargeable_uom());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if(offer.getShipment_level_rates() != null && offer.getShipment_level_rates().size() > 0)
            {
                for(FetchOffersResponse.AssociatedRate associatedRate : offer.getShipment_level_rates())
                {
                    if(associatedRate != null) {
                        associatedRate.setRates_uom(mapMeasurementBasis(associatedRate.getRates_uom()));
                        associatedRate.setTotal_unit_count(BigDecimal.ONE);
                        associatedRate.setMeasurement_unit("SHIPMENT");
                    }
                }
            }
        }
    }

    private NPMFetchOffersRequest createNPMOffersRequest(NPMFetchOffersRequestFromUI request) {
        Optional<CustomerBooking> customerBooking = Optional.empty();
        if(request.getBookingId() != null){
            customerBooking = customerBookingDao.findById(request.getBookingId());
        }
        boolean isAlteration = false;
        if (customerBooking!= null && customerBooking.isPresent() && !customerBooking.get().getBookingCharges().isEmpty()) {
            isAlteration = true;
        }

        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        String xBrowserTimeZone = TimeZoneConstants.DEFAULT_TIME_ZONE_ID;
        if (Objects.nonNull(requestAttributes)) {
            ServletRequestAttributes attributes = (ServletRequestAttributes) requestAttributes;
            xBrowserTimeZone = attributes.getRequest().getHeader(TimeZoneConstants.BROWSER_TIME_ZONE_NAME);
            if (StringUtils.isNotBlank(xBrowserTimeZone)) {
                xBrowserTimeZone = xBrowserTimeZone.replaceAll("\\s", "").trim().strip();
            }
        }
        String preferredDateInUTC = null;
        if(request.getPreferredDate() != null)
        {
            try {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
                LocalDateTime utcDate = DateUtils.convertDateFromUserTimeZone(LocalDateTime.parse(request.getPreferredDate(), formatter), xBrowserTimeZone, null, false);
                preferredDateInUTC = String.valueOf(utcDate.toLocalDate());
            }
            catch(Exception e)
            {
                log.error("Error in converting preferred date: {} to UTC", request.getPreferredDate());
                throw e;
            }
        }
        return NPMFetchOffersRequest.builder()
                .origin(request.getOrigin())
                .destination(request.getDestination())
                .POD(request.getPod())
                .POL(request.getPol())
                .exchange_rates(null)
                .currency(getCurrencyCode())
                .preferred_date(preferredDateInUTC)
                .preferred_date_type(request.getPreferredDateType())
                .carrier(NPMConstants.ANY) //hardcoded
                .loads_information(createLoadsInfo(request, customerBooking.isPresent() ? customerBooking.get() : null, isAlteration, NPMConstants.OFFERS_V2))
                .mode_of_transport(request.getModeOfTransport())
                .product_name(request.getCargoType()) // {TODO :: have to keep a mapping which is not present}
                .contract_details(createContractDetails(request))
                .shipment_type(request.getDirection() != null ? request.getDirection() : customerBooking.map(cb -> cb.getDirection()).orElse(null))
                .service_mode(request.getServiceMode())
                .fetch_default_rates(request.isFetchDefaultRates())
                .slab_rates(false)
                .scope_restriction(NPMConstants.SELL_COST_MARGIN)
                .service_category(null)
                .customer_category(null)
                .is_alteration(isAlteration)
                .offer_type(NPMConstants.CHEAPEST_OFFER_TYPE)
                .build();
    }

    private NPMFetchOffersRequest createNPMOffersV8Request(NPMFetchOffersRequestFromUI request) {
        Optional<CustomerBooking> customerBooking = Optional.empty();
        if(request.getBookingId() != null){
            customerBooking = customerBookingDao.findById(request.getBookingId());
        }
        boolean isAlteration = false;
        if (customerBooking!= null && customerBooking.isPresent() && !customerBooking.get().getBookingCharges().isEmpty()) {
            isAlteration = true;
        }

        return NPMFetchOffersRequest.builder()
                .origin(request.getOrigin())
                .destination(request.getDestination())
                .preferred_date(request.getPreferredDate())
                .preferred_date_type(request.getPreferredDateType())
                .loads_info(createLoadsInfo(request, customerBooking.isPresent() ? customerBooking.get() : null, false, NPMConstants.OFFERS_V8)) // TODO -; loads_info instead of loads_information
                .mode_of_transport(request.getModeOfTransport())
                .shipment_movement(customerBooking.map(cb -> cb.getDirection()).orElse(null))
                .service_mode(request.getServiceMode())
                .business_info(createBusinessInfo(request))
                .contracts_info(createContractInfo(request))
                .fetch_default_rates(request.isFetchDefaultRates())
                .carrier_code(request.getCarrierCode())
                .build();
    }

    private NPMFetchOffersRequest.ContractDetails createContractDetails(NPMFetchOffersRequestFromUI request) {
        if(request.getContractsInfo() == null) return null;
        return NPMFetchOffersRequest.ContractDetails.builder()
                .contracts(Objects.isNull(request.getContractsInfo().getContractId()) ? Arrays.asList() : Arrays.asList(request.getContractsInfo().getContractId()))
                .company_code(request.getContractsInfo().getCustomerOrgId())
                .build();
    }

    private NPMFetchOffersRequest.ContractsInfo createContractInfo(NPMFetchOffersRequestFromUI request) {
        return NPMFetchOffersRequest.ContractsInfo.builder()
                .customer_org_id(request.getContractsInfo() != null ? request.getContractsInfo().getCustomerOrgId() : null)
                .contract_id(request.getContractsInfo() != null?request.getContractsInfo().getContractId():null)
                .build();
    }

    private NPMFetchOffersRequest.BusinessInfo createBusinessInfo(NPMFetchOffersRequestFromUI request){
        return NPMFetchOffersRequest.BusinessInfo.builder()
                .product_name(request.getCargoType()).
                build();
    }

    private List<NPMFetchOffersRequest.LoadInformation> createLoadsInfo(NPMFetchOffersRequestFromUI request, CustomerBooking customerBooking, boolean isAlteration, String offerType) {
        //First Time
        List<NPMFetchOffersRequest.LoadInformation> result = new ArrayList<>();
        if (isAlteration == false) {
            var containers = request.getContainers() != null ? request.getContainers() : new ArrayList<NPMFetchOffersRequestFromUI.Container>();
            var packs = request.getPacks() != null ? request.getPacks() : new ArrayList<NPMFetchOffersRequestFromUI.Pack>();
            result.addAll(containers.stream().filter(Objects::nonNull).map(
                    c -> createLoadInfoFromContainers(request, c, offerType)).collect(Collectors.toList()));
            result.addAll(packs.stream().filter(Objects::nonNull).map(
                    p -> createLoadInfoFromPacks(request, p, offerType)).collect(Collectors.toList()));

            if((request.getPacks() == null || request.getPacks().size() == 0)
                    && (NPMConstants.AIR.equals(request.getModeOfTransport())  || NPMConstants.LCL.equals(request.getCargoType())))
            {
                 result.add(createLoadInfoForEmptyPacksList(request));
            }
            return result;
        }

        //otherwise : its second time : isAlteration = true

        Map<Long, Containers> existingContainers = customerBooking != null ? customerBooking.getContainersList().stream().filter(Objects::nonNull).collect(Collectors.toMap(Containers::getId, c -> c)) : new HashMap<>();
        Map<Long, Packing> existingPacks = customerBooking != null ? customerBooking.getPackingList().stream().filter(Objects::nonNull).collect(Collectors.toMap(Packing::getId, c -> c)) : new HashMap<>();

        if(request.getContainers() != null) {
            result.addAll(request.getContainers().stream().filter(Objects::nonNull).map(
                    c -> {
                        NPMFetchOffersRequest.LoadInformation model =
                                createLoadInfoFromContainers(request, c, offerType);
                        if (existingContainers.containsKey(c.getId())) {
                            Containers container = existingContainers.get(c.getId());
                            if (c.getQuantity() > container.getContainerCount())
                                model.getLoad_attributes().setDelta_quantity(c.getQuantity() - container.getContainerCount());
                            else
                                model.getLoad_attributes().setDelta_quantity(c.getQuantity());
                        }

                        return model;
                    }).collect(Collectors.toList()));
        }
        if(request.getPacks() != null) {

            result.addAll(request.getPacks().stream().filter(Objects::nonNull).map(
                    p -> {
                        NPMFetchOffersRequest.LoadInformation model =
                                createLoadInfoFromPacks(request, p, offerType);
                        if (existingPacks.containsKey(p.getId())) {
                            Packing packing = existingPacks.get(p.getId());
                            model.getLoad_attributes().setQuantity(Long.valueOf(packing.getPacks()));
                        }

                        return model;
                    }
            ).collect(Collectors.toList()));
        }

        return result;
    }

    private NPMFetchOffersRequest.LoadInformation createLoadInfoForEmptyPacksList(NPMFetchOffersRequestFromUI request) {
        return NPMFetchOffersRequest.LoadInformation.builder()
                .load_detail(NPMFetchOffersRequest.LoadDetail.builder()
                        .load_type(request.getCargoType())
                        .cargo_type(NPMConstants.ANY)
                        .product_category_code(NPMConstants.ANY)
                        .build())
                .load_attributes(NPMFetchOffersRequest.LoadAttributes.builder()
                        .volume(request.getVolume())
                        .volume_uom(request.getVolume_uom())
                        .weight(request.getWeight())
                        .weight_uom(request.getWeight_uom())
                        .build())
                .build();
    }

    private NPMFetchOffersRequest.LoadInformation createLoadInfoFromPacks(NPMFetchOffersRequestFromUI request, NPMFetchOffersRequestFromUI.Pack p,
                                                                          String offerType) {
        return NPMFetchOffersRequest.LoadInformation.builder()
                .load_detail(NPMFetchOffersRequest.LoadDetail.builder()
                        .load_type(request.getCargoType())
                        .cargo_type(p.getPackageType())
                        .product_category_code(NPMConstants.OFFERS_V2.equals(offerType)?p.getCommodity():null)
                        .commodity(NPMConstants.OFFERS_V8.equals(offerType)?p.getCommodity():null)
                        .build())
                .load_attributes(NPMFetchOffersRequest.LoadAttributes.builder()
                        .chargeable(p.getChargeable())
                        .chargeable_uom(p.getChargeableUnit())
                        .volume(p.getVolume())
                        .volume_uom(p.getVolumeUnit())
                        .weight(p.getWeight())
                        .weight_uom(p.getWeightUnit())
                        .quantity(p.getQuantity())
                        .quantity_uom(request.getCargoType().equals(NPMConstants.FCL) ? NPMConstants.UNIT : p.getPackageType())
                        .delta_quantity(p.getQuantity())
                        .build())
                .build();
    }

    private NPMFetchOffersRequest.LoadInformation createLoadInfoFromContainers(NPMFetchOffersRequestFromUI request,
                                                                               NPMFetchOffersRequestFromUI.Container containerFromRequest,
                                                                               String offerType) {
        return NPMFetchOffersRequest.LoadInformation.builder()
                .load_detail(NPMFetchOffersRequest.LoadDetail.builder()
                        .load_type(request.getCargoType())
                        .cargo_type(containerFromRequest.getContainerType())
                        .product_category_code(NPMConstants.OFFERS_V2.equals(offerType)? containerFromRequest.getCommodityCode() : null)
                        .commodity(NPMConstants.OFFERS_V8.equals(offerType)? containerFromRequest.getCommodityCode() : null)
                        .build())
                .load_attributes(NPMFetchOffersRequest.LoadAttributes.builder()
                        .delta_quantity(containerFromRequest.getQuantity())
                        .quantity(containerFromRequest.getQuantity())
                        .quantity_uom(request.getCargoType().equals(NPMConstants.FCL) ? NPMConstants.UNIT : containerFromRequest.getContainerType())
                        .weight(containerFromRequest.getGrossWeight())
                        .weight_uom(containerFromRequest.getGrossWeightUnit())
                        .build())
                .build();
    }
    @Override
    public NPMFetchLangChargeCodeResponse fetchMultiLangChargeCode(CommonRequestModel commonRequestModel) throws RunnerException {
        try {
            NPMFetchMultiLangChargeCodeRequest request = (NPMFetchMultiLangChargeCodeRequest) commonRequestModel.getData();
            String url = npmBaseUrl + npmMultiLangChargeCode;
            log.info("{}" + PAYLOAD_SENT_FOR_EVENT_WITH_REQUEST_PAYLOAD_MSG, LoggerHelper.getRequestIdFromMDC(), IntegrationType.NPM_FETCH_MULTI_LANG_CHARGE_CODE, jsonHelper.convertToJson(request));
            ResponseEntity<NPMFetchLangChargeCodeResponse> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)), NPMFetchLangChargeCodeResponse.class);
            log.info("{}" + PAYLOAD_SENT_FOR_EVENT_WITH_REQUEST_PAYLOAD_MSG, LoggerHelper.getRequestIdFromMDC(), IntegrationType.NPM_FETCH_MULTI_LANG_CHARGE_CODE, jsonHelper.convertToJson(response.getBody()));
            return response.getBody();
        } catch (HttpStatusCodeException ex) {
            NpmErrorResponse npmErrorResponse = jsonHelper.readFromJson(ex.getResponseBodyAsString(), NpmErrorResponse.class);
            log.error("NPM Fetch MultiLang Charge Code failed due to: {}", jsonHelper.convertToJson(npmErrorResponse));
            throw new NPMException("Error from NPM while fetching MultiLang Charge Code: " + npmErrorResponse.getErrorMessage());
        }
    }

}
