package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
@EnableAsync
public class PackingSync implements IPackingSync {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    RestTemplate restTemplate;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.bulkPackingSync}")
    private String BULK_PACKING_SYNC_URL;

    @Override
    @Async("asyncExecutor")
    public ResponseEntity<?> sync(List<Packing> packings, Long consolidationId, Long shipmentId) {
        List<PackingRequestV2> requestV2List = new ArrayList<>();
        for (var packing : packings) {
            requestV2List.add(modelMapper.map(packing, PackingRequestV2.class));
        }
        BulkPackingRequestV2 packingRequestV2 = BulkPackingRequestV2.builder()
                .bulkPacking(requestV2List).ConsolidationId(consolidationId).ShipmentId(shipmentId).build();
        String finalCs = jsonHelper.convertToJson(packingRequestV2);
        var resp = retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            HttpEntity<V1DataResponse> entity = new HttpEntity(finalCs, V1AuthHelper.getHeaders());
            var response = this.restTemplate.postForEntity(this.BULK_PACKING_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
            return response;
        });
        return ResponseHelper.buildSuccessResponse(resp);
    }
}
