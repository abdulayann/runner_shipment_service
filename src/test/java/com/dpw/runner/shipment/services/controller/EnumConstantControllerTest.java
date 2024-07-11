package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IEnumConstantService;
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
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {EnumConstantController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EnumConstantControllerTest {

    @Mock
    private IEnumConstantService enumConstantService;
    @InjectMocks
    private EnumConstantController enumConstantController;

    @Test
    void list() {
        // Mock
        when(enumConstantService.list()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = enumConstantController.list();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }




}
