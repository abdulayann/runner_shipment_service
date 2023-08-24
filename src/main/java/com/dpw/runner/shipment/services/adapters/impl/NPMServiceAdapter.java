package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class NPMServiceAdapter implements INPMServiceAdapter {

    @Value("${NPM.BaseUrl}")
    private String npmBaseUrl;
    @Value("${NPM.Contracts}")
    private String npmContracts;
    @Value("${NPM.xApikeyV2}")
    private String xApikeyV2;
    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Override
    public ResponseEntity<?> fetchContracts(CommonRequestModel commonRequestModel) throws Exception {
        ListContractRequest listContractRequest = (ListContractRequest) commonRequestModel.getData();
        String url = npmBaseUrl + npmContracts;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("x-api-key-v2", xApikeyV2);

        HttpEntity<Object> request = new HttpEntity<Object>(listContractRequest, headers);

        ResponseEntity<?> response = restTemplate.exchange(url, HttpMethod.POST, request, Object.class);
        return response;
    }

    public ResponseEntity<?> updateContracts(UpdateContractRequest updateContractRequest) throws Exception {
        String url = npmBaseUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("x-api-key-v2", xApikeyV2);

        HttpEntity<Object> request = new HttpEntity<Object>(updateContractRequest, headers);

        ResponseEntity<?> response = restTemplate.exchange(url, HttpMethod.POST, request, Object.class);
        return response;
    }

    @Override
    public ResponseEntity<?> fetchOffers(CommonRequestModel req) throws Exception {
        String url = npmBaseUrl; //TODO :: write the proper url
        NPMFetchOffersRequestFromUI fetchOffersRequest = (NPMFetchOffersRequestFromUI) req.getData();
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("x-api-key-v2", xApikeyV2);

        HttpEntity<Object> request = new HttpEntity<>(createNPMOffersRequest(fetchOffersRequest), headers);
        ResponseEntity<?> response = restTemplate.exchange(url, HttpMethod.GET, request, Object.class);
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
                .POD(request.getPol())
                .POL(request.getPod())
                .exchange_rates(null)
                .preferred_date(request.getPreferredDate())
                .preferred_date_type(request.getPreferredDateType())
                .carrier("ANY") //hardcoded
                .loads_information(createLoadsInfo(request, customerBooking.get(), isAlteration))
                .mode_of_transport(request.getModeOfTransport())
                .product_name(request.getCargoType()) // {TODO :: have to keep a mapping which is not present}
                .contract_details(createContractDetails(request))
                .shipment_type(customerBooking.get().getDirection())
                .service_mode(request.getServiceMode())
                .fetch_default_rates(request.isFetchDefaultRates())
                .slab_rates(false)
                .scope_restriction("SELL_COST_MARGIN")
                .service_category(null)
                .customer_category(null)
                .is_alteration(isAlteration)
                .offer_type("CHEAPEST")
                .build();
    }

    private NPMFetchOffersRequest.ContractDetails createContractDetails(NPMFetchOffersRequestFromUI request) {
        return NPMFetchOffersRequest.ContractDetails.builder()
                .contracts(Collections.singletonList(request.getContractsInfo().getContractId()))
                .company_code(null)
                .build();
    }

    private List<NPMFetchOffersRequest.LoadInformation> createLoadsInfo(NPMFetchOffersRequestFromUI request, CustomerBooking customerBooking, boolean isAlteration) {
        //First Time
        List<NPMFetchOffersRequest.LoadInformation> result = new ArrayList<>();
        if (isAlteration == false) {
            var containers = request.getContainers();
            var packs = request.getPacks();
            result.addAll(containers.stream().map(
                    c -> createLoadInfoFromContainers(request, c)).collect(Collectors.toList()));
            result.addAll(packs.stream().map(
                    p -> createLoadInfoFromPacks(request, p)).collect(Collectors.toList()));
            return result;
        }

        //otherwise : its second time : isAlteration = true

        Map<Long, Containers> existingContainers = customerBooking.getContainersList().stream().collect(Collectors.toMap(Containers::getId, c -> c));
        Map<Long, Packing> existingPacks = customerBooking.getPackingList().stream().collect(Collectors.toMap(Packing::getId, c -> c));

        result.addAll(request.getContainers().stream().map(
                c -> {
                    NPMFetchOffersRequest.LoadInformation model =
                            createLoadInfoFromContainers(request, c);
                    if (existingContainers.containsKey(c.getId())) {
                        Containers container = existingContainers.get(c.getId());
                        if (c.getQuantity() > container.getContainerCount())
                            model.getLoad_attributes().setDelta_quantity(c.getQuantity() - container.getContainerCount());
                        else
                            model.getLoad_attributes().setDelta_quantity(c.getQuantity());
                    }

                    return model;
                }).collect(Collectors.toList()));

        result.addAll(request.getPacks().stream().map(
                p -> {
                    NPMFetchOffersRequest.LoadInformation model =
                            createLoadInfoFromPacks(request, p);
                    if (existingPacks.containsKey(p.getId())) {
                        Packing packing = existingPacks.get(p.getId());
                        model.getLoad_attributes().setQuantity(Long.valueOf(packing.getPacks()));
                    }

                    return model;
                }
        ).collect(Collectors.toList()));

        return result;
    }

    private NPMFetchOffersRequest.LoadInformation createLoadInfoFromPacks(NPMFetchOffersRequestFromUI request, NPMFetchOffersRequestFromUI.Pack p) {
        return NPMFetchOffersRequest.LoadInformation.builder()
                .load_detail(NPMFetchOffersRequest.LoadDetail.builder()
                        .load_type(request.getCargoType())
                        .cargo_type(p.getPackageType())
                        .product_category_code(p.getCommodity())
                        .build())
                .load_attributes(NPMFetchOffersRequest.LoadAttributes.builder()
                        .chargeable(p.getChargeable())
                        .chargeable_uom(p.getChargeableUnit())
                        .volume(p.getVolume())
                        .volume_uom(p.getVolumeUnit())
                        .weight(p.getWeight())
                        .weight_uom(p.getWeightUnit())
                        .quantity(p.getQuantity())
                        .quantity_uom("unit")
                        .delta_quantity(p.getQuantity())
                        .build())
                .build();
    }

    private NPMFetchOffersRequest.LoadInformation createLoadInfoFromContainers(NPMFetchOffersRequestFromUI request,
                                                                               NPMFetchOffersRequestFromUI.Container containerFromRequest) {
        return NPMFetchOffersRequest.LoadInformation.builder()
                .load_detail(NPMFetchOffersRequest.LoadDetail.builder()
                        .load_type(request.getCargoType())
                        .cargo_type(containerFromRequest.getContainerType())
                        .product_category_code(containerFromRequest.getCommodityCode())
                        .build())
                .load_attributes(NPMFetchOffersRequest.LoadAttributes.builder()
                        .delta_quantity(containerFromRequest.getQuantity())
                        .quantity(containerFromRequest.getQuantity())
                        .quantity_uom("unit")
                        .weight(containerFromRequest.getGrossWeight())
                        .weight_uom(containerFromRequest.getGrossWeightUnit())
                        .build())
                .build();
    }

}
