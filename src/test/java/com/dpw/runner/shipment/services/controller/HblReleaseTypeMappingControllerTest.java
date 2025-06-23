package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.HblReleaseTypeMappingListRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblReleaseTypeMappingService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {HblReleaseTypeMappingControllerTest.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HblReleaseTypeMappingControllerTest {

    @Mock
    private IHblReleaseTypeMappingService hblReleaseTypeMappingService;
    @InjectMocks
    private HblReleaseTypeMappingController releaseTypeMappingControllerTest;

    @Test
    void list() {
        // Mock
        when(hblReleaseTypeMappingService.retrieveByHblIdAndReleaseType(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = releaseTypeMappingControllerTest.list(new HblReleaseTypeMappingListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }




}
