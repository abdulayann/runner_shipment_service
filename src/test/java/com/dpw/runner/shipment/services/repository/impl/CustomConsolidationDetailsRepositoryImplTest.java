package com.dpw.runner.shipment.services.repository.impl;


import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Collections;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.response.Consolidation.ConsolidationLiteResponse;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

@ExtendWith(MockitoExtension.class)
public class CustomConsolidationDetailsRepositoryImplTest {

  @Mock
  private EntityManager entityManager;


  @InjectMocks
  private CustomConsolidationDetailsRepositoryImpl repository;

  @BeforeEach
  void setup() {

    CriteriaBuilder cb = mock(CriteriaBuilder.class);

    when(entityManager.getCriteriaBuilder()).thenReturn(cb);
    CriteriaQuery cq = mock(CriteriaQuery.class);
    when(cb.createQuery(eq(ConsolidationLiteResponse.class))).thenReturn(cq);
    Root root = mock(Root.class);
    when(cq.from(eq(ConsolidationDetails.class))).thenReturn(root);
    Join join = mock(Join.class);
    when(root.join("carrierDetails", JoinType.LEFT)).thenReturn(join);

    when(entityManager.createQuery(cq)).thenReturn(mock(TypedQuery.class));


    CriteriaQuery criteriaQuery = mock(CriteriaQuery.class);
    when(cb.createQuery(Long.class)).thenReturn(criteriaQuery);

    when(criteriaQuery.from(eq(ConsolidationDetails.class))).thenReturn(root);

    TypedQuery tq = mock(TypedQuery.class);
    when(entityManager.createQuery(criteriaQuery)).thenReturn(tq);
    when(tq.getSingleResult()).thenReturn(10l);

  }

  @Test
  void testFindAllLiteConsol_withValidSpecification() {
    // Arrange
    Specification<ConsolidationDetails> spec = mock(Specification.class);
    Pageable pageable = PageRequest.of(0, 10);

    ConsolidationLiteResponse mockResponse = new ConsolidationLiteResponse();
    List<ConsolidationLiteResponse> mockResultList = Collections.singletonList(mockResponse);

    // Act
    Page<ConsolidationLiteResponse> result = repository.findAllLiteConsol(spec, pageable);

    // Assert
    assertNotNull(result);
    assertEquals(10, result.getTotalElements());
  }

}

