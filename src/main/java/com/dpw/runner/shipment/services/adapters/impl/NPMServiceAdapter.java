package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class NPMServiceAdapter implements INPMServiceAdapter {

    @Value("${NPM.BaseUrl}")
    private String npmBaseUrl;

    @Value("${NPM.Contracts}")
    private String npmContracts;

    @Value("${NPM.Offers}")
    private String npmOffersUrl;

    @Value("${NPM.OffersV8}")
    private String npmOffersV8Url;


    @Value("${NPM.Update}")
    private String npmUpdateUrl;

    private final RestTemplate restTemplate;

    @Autowired
    public NPMServiceAdapter(@Qualifier("restTemplateForNPM") RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Override
    public ResponseEntity<?> fetchContracts(CommonRequestModel commonRequestModel) throws Exception {
        ListContractRequest listContractRequest = (ListContractRequest) commonRequestModel.getData();
        String url = npmBaseUrl + npmContracts;
        ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(new ObjectMapper().writeValueAsString(listContractRequest)), Object.class);
        return response;
    }

    @Override
    public ResponseEntity<?> updateContracts(CommonRequestModel commonRequestModel) throws Exception {
        UpdateContractRequest updateContractRequest = (UpdateContractRequest) commonRequestModel.getData();
        String url = npmBaseUrl + npmUpdateUrl;
        ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(updateContractRequest), Object.class);
        return response;
    }

    @Override
    public ResponseEntity<?> fetchOffers(CommonRequestModel req) throws Exception {
        String url = npmBaseUrl + npmOffersUrl;
        NPMFetchOffersRequestFromUI fetchOffersRequest = (NPMFetchOffersRequestFromUI) req.getData();
        ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(new ObjectMapper().writeValueAsString(createNPMOffersRequest(fetchOffersRequest))), Object.class);
        return response;
    }

    @Override
    public ResponseEntity<?> fetchOffersV8(CommonRequestModel req) throws Exception {
        String url = npmBaseUrl + npmOffersV8Url;
        NPMFetchOffersRequestFromUI fetchOffersRequest = (NPMFetchOffersRequestFromUI) req.getData();
        ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).body(new ObjectMapper().writeValueAsString(createNPMOffersV8Request(fetchOffersRequest))), Object.class);
        return response;
    }

    private NPMFetchOffersRequest createNPMOffersRequest(NPMFetchOffersRequestFromUI request) {
        Optional<CustomerBooking> customerBooking = customerBookingDao.findById(request.getBookingId());
        boolean isAlteration = false;
        if (customerBooking.isPresent() && !customerBooking.get().getBookingCharges().isEmpty()) {
            isAlteration = true;
        }

        return NPMFetchOffersRequest.builder()
                .origin(request.getOrigin())
                .destination(request.getDestination())
                .POD(request.getPod())
                .POL(request.getPol())
                .exchange_rates(null)
                .preferred_date(request.getPreferredDate())
                .preferred_date_type(request.getPreferredDateType())
                .carrier(NPMConstants.ANY) //hardcoded
                .loads_information(createLoadsInfo(request, customerBooking.get(), isAlteration, NPMConstants.OFFERS_V2))
                .mode_of_transport(request.getModeOfTransport())
                .product_name(request.getCargoType()) // {TODO :: have to keep a mapping which is not present}
                .contract_details(createContractDetails(request))
                .shipment_type(customerBooking.map(cb -> cb.getDirection()).orElse(null))
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
        Optional<CustomerBooking> customerBooking = customerBookingDao.findById(request.getBookingId());
        boolean isAlteration = false;
        if (customerBooking.isPresent() && !customerBooking.get().getBookingCharges().isEmpty()) {
            isAlteration = true;
        }

        return NPMFetchOffersRequest.builder()
                .origin(request.getOrigin())
                .destination(request.getDestination())
                .preferred_date(request.getPreferredDate())
                .preferred_date_type(request.getPreferredDateType())
                .loads_info(createLoadsInfo(request, customerBooking.get(), false, NPMConstants.OFFERS_V8)) // TODO -; loads_info instead of loads_information
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
        return NPMFetchOffersRequest.ContractDetails.builder()
                .contracts(Collections.singletonList(request.getContractsInfo().getContractId()))
                .company_code(null)
                .build();
    }

    private NPMFetchOffersRequest.ContractsInfo createContractInfo(NPMFetchOffersRequestFromUI request) {
        return NPMFetchOffersRequest.ContractsInfo.builder()
                .customer_org_id(request.getContractsInfo().getCustomerOrgId())
                .contract_id(request.getContractsInfo().getContractId())
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
            var containers = request.getContainers();
            var packs = request.getPacks();
            result.addAll(containers.stream().filter(Objects::nonNull).map(
                    c -> createLoadInfoFromContainers(request, c, offerType)).collect(Collectors.toList()));
            result.addAll(packs.stream().filter(Objects::nonNull).map(
                    p -> createLoadInfoFromPacks(request, p, offerType)).collect(Collectors.toList()));
            return result;
        }

        //otherwise : its second time : isAlteration = true

        Map<Long, Containers> existingContainers = customerBooking != null ? customerBooking.getContainersList().stream().filter(Objects::nonNull).collect(Collectors.toMap(Containers::getId, c -> c)) : new HashMap<>();
        Map<Long, Packing> existingPacks = customerBooking != null ? customerBooking.getPackingList().stream().filter(Objects::nonNull).collect(Collectors.toMap(Packing::getId, c -> c)) : new HashMap<>();

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

        return result;
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

}
