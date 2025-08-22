package com.dpw.runner.shipment.services.migration.utils;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.exception.exceptions.MdmException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

@Slf4j
@Component
@Generated
public class MigrationUtil {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IIntegrationResponseDao integrationResponseDao;

    @Autowired
    private MDMServiceAdapter mdmServiceAdapter;

    private MigrationUtil() {}

    public static List<Long> collectAllProcessedIds(List<Future<Long>> queue) {
        List<Long> ids = new ArrayList<>();
        for (Future<Long> future : queue) {
            try {
                ids.add(future.get());
            } catch (Exception er) {
                log.error("Error in trx", er);
            }
        }
        return ids;
    }

    public void saveErrorResponse(Long entityId, String entityType, IntegrationType integrationType, Status status, String message) {
        IntegrationResponse response = IntegrationResponse.builder()
                .entityId(entityId).entityType(entityType).integrationType(integrationType).status(status)
                .response_message(jsonHelper.convertToJson(message))
                .build();
        integrationResponseDao.save(response);
    }

    public Map<String, BigDecimal> initCodeTeuMap() {
        try {
            DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
            List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(
                    mdmResponse.getData(), MdmContainerTypeResponse.class
            );
            return containerTypes.stream()
                    .collect(Collectors.toUnmodifiableMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
        } catch (RunnerException ex) {
            throw new MdmException(ex.getMessage());
        }
    }

    public static void futureCompletion(List<CompletableFuture<Object>> futures) {

        CompletableFuture<Void> completableFuture = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        completableFuture.exceptionally(ex -> {
            throw new IllegalArgumentException(ex);
        }).join();
    }
}
