package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICommonErrorLogsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CommonErrorLogsControllerTest {
    @Mock
    private ICommonErrorLogsService commonErrorLogsService;
    @InjectMocks
    private CommonErrorLogsController commonErrorLogsController;

    @Test
    void list() {
        when(commonErrorLogsService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = commonErrorLogsController.list(ListCommonRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        when(commonErrorLogsService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = commonErrorLogsController.retrieveById(Optional.of(1L), Optional.of("893cc8fa-7315-4d23-a635-3ce8705a5140"));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}