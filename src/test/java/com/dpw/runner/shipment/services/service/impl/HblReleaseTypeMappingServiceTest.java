package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.reportingservice.Models.DocumentRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.ApiError;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IHblReleaseTypeMappingDao;

import java.util.ArrayList;
import java.util.List;

import com.dpw.runner.shipment.services.dto.request.HblReleaseTypeMappingListRequest;
import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ContextConfiguration(classes = {HblReleaseTypeMappingService.class})
@ExtendWith(SpringExtension.class)
class HblReleaseTypeMappingServiceTest {
    @Autowired
    private HblReleaseTypeMappingService hblReleaseTypeMappingService;

    @MockBean
    private IHblReleaseTypeMappingDao iHblReleaseTypeMappingDao;

    @MockBean
    private ModelMapper modelMapper;

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#create(CommonRequestModel)}
     */
    @Test
    void testCreate() {
        // Arrange, Act and Assert
        assertNull(hblReleaseTypeMappingService.create(null));
        assertNull(hblReleaseTypeMappingService.create(mock(CommonRequestModel.class)));
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#update(CommonRequestModel)}
     */
    @Test
    void testUpdate() {
        // Arrange, Act and Assert
        assertNull(hblReleaseTypeMappingService.update(null));
        assertNull(hblReleaseTypeMappingService.update(mock(CommonRequestModel.class)));
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#list(CommonRequestModel)}
     */
    @Test
    void testList() {
        // Arrange, Act and Assert
        assertNull(hblReleaseTypeMappingService.list(null));
        assertNull(hblReleaseTypeMappingService.list(mock(CommonRequestModel.class)));
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#listAsync(CommonRequestModel)}
     */
    @Test
    void testListAsync() {
        // Arrange, Act and Assert
        assertNull(hblReleaseTypeMappingService.listAsync(null));
        assertNull(hblReleaseTypeMappingService.listAsync(mock(CommonRequestModel.class)));
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#delete(CommonRequestModel)}
     */
    @Test
    void testDelete() {
        // Arrange, Act and Assert
        assertNull(hblReleaseTypeMappingService.delete(null));
        assertNull(hblReleaseTypeMappingService.delete(mock(CommonRequestModel.class)));
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveById(CommonRequestModel)}
     */
    @Test
    void testRetrieveById() {
        // Arrange, Act and Assert
        assertNull(hblReleaseTypeMappingService.retrieveById(null));
        assertNull(hblReleaseTypeMappingService.retrieveById(mock(CommonRequestModel.class)));
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType() {
        // Arrange and Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(null);

        // Assert
        ApiError error = ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getError();
        assertEquals(
                "Cannot invoke \"com.dpw.runner.shipment.services.commons.requests.CommonRequestModel.getData()\" because"
                        + " \"commonRequestModel\" is null",
                error.getMessage());
        assertNull(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getRequestId());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).isSuccess());
        assertTrue(actualRetrieveByHblIdAndReleaseTypeResult.hasBody());
        assertTrue(actualRetrieveByHblIdAndReleaseTypeResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType2() {
        // Arrange
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(commonRequestModel);

        // Assert
        ApiError error = ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getError();
        assertNull(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getRequestId());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).isSuccess());
        assertTrue(actualRetrieveByHblIdAndReleaseTypeResult.hasBody());
        assertTrue(actualRetrieveByHblIdAndReleaseTypeResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType3() {
        // Arrange
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(commonRequestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        ApiError error = ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getError();
        assertEquals("Invalid request for fetching original printed, make sure mandatory fields are available: HBL Id &"
                + " Release Type", error.getMessage());
        assertNull(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getRequestId());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualRetrieveByHblIdAndReleaseTypeResult.getBody()).isSuccess());
        assertTrue(actualRetrieveByHblIdAndReleaseTypeResult.hasBody());
        assertTrue(actualRetrieveByHblIdAndReleaseTypeResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType4() {
        // Arrange
        var hblRequest = HblReleaseTypeMappingListRequest.builder().hblId(1l).releaseType("OBL").build();
        when(iHblReleaseTypeMappingDao.findByReleaseTypeAndHblId(any(), anyString())).thenReturn(List.of(HblReleaseTypeMapping.builder().build()));
        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(CommonRequestModel.buildRequest(hblRequest));

        // Assert
        assertEquals(HttpStatus.OK, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType5() {
        // Arrange
        var hblRequest = HblReleaseTypeMappingListRequest.builder().releaseType("OBL").build();
        when(iHblReleaseTypeMappingDao.findByReleaseTypeAndHblId(any(), anyString())).thenReturn(List.of(HblReleaseTypeMapping.builder().build()));
        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(CommonRequestModel.buildRequest(hblRequest));

        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType6() {
        // Arrange
        var hblRequest = HblReleaseTypeMappingListRequest.builder().hblId(1L).build();
        when(iHblReleaseTypeMappingDao.findByReleaseTypeAndHblId(any(), anyString())).thenReturn(List.of(HblReleaseTypeMapping.builder().build()));
        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(CommonRequestModel.buildRequest(hblRequest));

        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
    }

    /**
     * Method under test:
     * {@link HblReleaseTypeMappingService#retrieveByHblIdAndReleaseType(CommonRequestModel)}
     */
    @Test
    void testRetrieveByHblIdAndReleaseType7() {
        // Arrange
        var hblRequest = HblReleaseTypeMappingListRequest.builder().hblId(1L).releaseType("OBL").build();
        when(iHblReleaseTypeMappingDao.findByReleaseTypeAndHblId(any(), anyString())).thenThrow(new RuntimeException());
        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveByHblIdAndReleaseTypeResult = hblReleaseTypeMappingService
                .retrieveByHblIdAndReleaseType(CommonRequestModel.buildRequest(hblRequest));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, actualRetrieveByHblIdAndReleaseTypeResult.getStatusCode());
    }
}
