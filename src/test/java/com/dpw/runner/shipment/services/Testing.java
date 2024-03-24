package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helper.TokenFetcher;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@RunWith(SpringRunner.class)
@TestPropertySource("classpath:application-qa.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class Testing {
    @Autowired
    private V1ServiceImpl v1Service;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private TokenFetcher tokenFetcher;


    @Test
    @Disabled
    void test(){
        UserContext.setUser(UsersDto.builder().UserId(238L)
                .Username("hipl2")
                .DisplayName("hipl2")
                .Email("mnsuppal2@gmail.com")
                .build()
        );

        RequestAuthContext.setAuthToken(tokenFetcher.fetchUserToken());

        List<String> shipmentIdList = List.of("ADAS24010019");
        for(var shipmentNumber : shipmentIdList) {
            var shipmentFromV1 = v1Service.getShipment(V1RetrieveRequest.builder().EntityId(shipmentNumber).build());
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentNumber, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
            var shipmnentFromV2 = shipmentDetails.getContent().get(0);


        }

    }


}
