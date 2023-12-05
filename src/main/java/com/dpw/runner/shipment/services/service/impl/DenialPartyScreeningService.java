package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.DenialParty.request.SearchEntity;
import com.dpw.runner.shipment.services.dto.DenialParty.request.SearchEntityRequest;
import com.dpw.runner.shipment.services.dto.request.DenialPartySearchEntityRequest;
import com.dpw.runner.shipment.services.dto.response.DenialPartySearchResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.descarts.IDescartsService;
import com.dpw.runner.shipment.services.service.descarts.impl.DescartsService;
import com.dpw.runner.shipment.services.service.interfaces.IDenialPartyScreeningService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.map.MultiValueMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Slf4j
@Service
public class DenialPartyScreeningService implements IDenialPartyScreeningService {

    @Autowired
    private RestTemplate restTemplate;
    @Autowired
    private IDescartsService descartsService;
    @Value("${descarts.service.searchentity.url}")
    private String EXTERNAL_API_URL;
    @Value("${descarts.service.searchtype}")
    private String DescartSearchType;
    @Value("${descarts.service.ssecno}")
    private String DescartSsecNo;
    @Value("${descarts.service.password}")
    private String Descartpassword;
    @Override
    public ResponseEntity<?> createRequestAndSearchEntity(DenialPartySearchEntityRequest commonRequestModel) {

        SearchEntity searchQuery = new SearchEntity();
        searchQuery.setSname(commonRequestModel.getName());
        searchQuery.setScompany(commonRequestModel.getCompany());
        searchQuery.setScity(commonRequestModel.getCity());
        searchQuery.setScountry(commonRequestModel.getCountry());
        searchQuery.setSstate(commonRequestModel.getState());
        searchQuery.setSzip(commonRequestModel.getZip());
        searchQuery.setSaddress1(commonRequestModel.getAddress1());
        searchQuery.setSaddress2(commonRequestModel.getAddress2());
        searchQuery.setSaddress2(commonRequestModel.getAddress2());

        List<SearchEntity> queryList = new ArrayList<>();
        queryList.add(searchQuery);
        SearchEntityRequest finalSearchQuery = new SearchEntityRequest();
        finalSearchQuery.set__type(DescartSearchType);
        finalSearchQuery.setSsecno(DescartSsecNo);
        finalSearchQuery.setSpassword(Descartpassword);
        finalSearchQuery.setSearches(queryList);
        return descartsService.searchEntity(finalSearchQuery);

    }
}
