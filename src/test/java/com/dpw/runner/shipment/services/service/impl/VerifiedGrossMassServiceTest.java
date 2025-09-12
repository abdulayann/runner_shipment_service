package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonContainersRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class VerifiedGrossMassServiceTest {

    @Mock
    private ICommonContainersRepository commonContainersRepository;

    @InjectMocks
    private VerifiedGrossMassService verifiedGrossMassService;
    @Mock
    private JsonHelper jsonHelper;

    @Test
    public void bulkUpdateContainers_ShouldThrowValidationException_WhenLessThanTwoIds() {
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(List.of(1L)); // only 1 id
        assertThrows(ValidationException.class, () ->
                verifiedGrossMassService.bulkUpdateContainers(request)
        );
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateSuccessfully() {
        // Arrange
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));
        request.setApprovalSignature("abc");
        // create weighingParty
        Parties party = new Parties();
        party.setId(1001L);
        party.setOrgCode("ORG123");
        party.setType("SHIPPER");
        request.setWeighingParty(party);

        CommonContainers c1 = new CommonContainers();
        c1.setId(1L);
        CommonContainers c2 = new CommonContainers();
        c2.setId(2L);
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(containers);
        when(commonContainersRepository.saveAll(containers))
                .thenReturn(containers);

        // Mock JsonHelper conversion
        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenAnswer(invocation -> {
                    CommonContainers c = invocation.getArgument(0);
                    CommonContainerResponse r = new CommonContainerResponse();
                    r.setId(c.getId());
                    r.setApprovalSignature(c.getApprovalSignature());
                    r.setWeighingParty(c.getWeighingParty());
                    return r;
                });

        // Act
        List<CommonContainerResponse> result = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals(2, result.size());
        assertEquals("ABC", result.get(0).getApprovalSignature()); // signature should be uppercase
        assertEquals("ORG123", result.get(0).getWeighingParty().getOrgCode());
    }


    @Test
    void bulkUpdateContainers_ShouldThrowValidationException_WhenContainerNotFound() {
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(Arrays.asList(1L, 2L));

        when(commonContainersRepository.findAllByIdIn(request.getContainerIds()))
                .thenReturn(Collections.singletonList(new CommonContainers())); // only 1 returned

        assertThrows(ValidationException.class,
                () -> verifiedGrossMassService.bulkUpdateContainers(request));
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateApprovalDate() {
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setApprovalDate(LocalDateTime.of(2025, 1, 1, 12, 0));

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        assertEquals(LocalDateTime.of(2025, 1, 1, 12, 0), containers.get(0).getApprovalDate());
        assertEquals(LocalDateTime.of(2025, 1, 1, 12, 0), containers.get(1).getApprovalDate());
        assertEquals(2, responses.size());
    }

    @Test
    void bulkUpdateContainers_ShouldUpdateOnlyProvidedFields() {
        // Arrange
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setApprovalSignature("john"); // only updating signature

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals("JOHN", containers.get(0).getApprovalSignature()); // uppercase applied
        assertEquals("JOHN", containers.get(1).getApprovalSignature());
        assertEquals(2, responses.size());
    }
    @Test
    void bulkUpdateContainers_ShouldUpdateWeightDeterminationFields() {
        // Arrange
        List<Long> ids = Arrays.asList(1L, 2L);
        VerifiedGrossMassBulkUpdateRequest request = new VerifiedGrossMassBulkUpdateRequest();
        request.setContainerIds(ids);
        request.setWeightDeterminationMethod(WeightDeterminationMethodType.METHOD1);
        request.setWeightDeterminationLocation("Port of LA");

        CommonContainers c1 = new CommonContainers();
        CommonContainers c2 = new CommonContainers();
        List<CommonContainers> containers = Arrays.asList(c1, c2);

        when(commonContainersRepository.findAllByIdIn(ids)).thenReturn(containers);
        when(commonContainersRepository.saveAll(anyList())).thenReturn(containers);

        when(jsonHelper.convertValue(any(CommonContainers.class), eq(CommonContainerResponse.class)))
                .thenReturn(new CommonContainerResponse());

        // Act
        List<CommonContainerResponse> responses = verifiedGrossMassService.bulkUpdateContainers(request);

        // Assert
        assertEquals(WeightDeterminationMethodType.METHOD1, c1.getWeightDeterminationMethod());
        assertEquals("Port of LA", c1.getWeightDeterminationLocation());
        assertEquals(WeightDeterminationMethodType.METHOD1, c2.getWeightDeterminationMethod());
        assertEquals("Port of LA", c2.getWeightDeterminationLocation());
        assertEquals(2, responses.size());

        // Verify saveAll called once
        verify(commonContainersRepository, times(1)).saveAll(containers);
    }
}
