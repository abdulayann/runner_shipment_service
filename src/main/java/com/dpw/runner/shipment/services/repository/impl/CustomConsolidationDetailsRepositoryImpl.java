package com.dpw.runner.shipment.services.repository.impl;


import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.response.consolidation.ConsolidationLiteResponse;;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomConsolidationDetailsRepository;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

@Repository
public class CustomConsolidationDetailsRepositoryImpl implements ICustomConsolidationDetailsRepository {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<ConsolidationLiteResponse> findAllLiteConsol(Specification<ConsolidationDetails> spec, Pageable pageable) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<ConsolidationLiteResponse> cq = cb.createQuery(ConsolidationLiteResponse.class);
    Root<ConsolidationDetails> root = cq.from(ConsolidationDetails.class);
    Join<ConsolidationDetails, CarrierDetails> carrier = root.join("carrierDetails", JoinType.LEFT);

    // Apply the specification filters
    if (spec != null) {
      Predicate predicate = spec.toPredicate(root, cq, cb);
      if (predicate != null) {
        cq.where(predicate);
      } else {
        cq.where(cb.conjunction());
      }
    } else {
      cq.where(cb.conjunction());
    }

    // Add your selects (mapping all fields to ConsolidationLiteResponse)
    cq.select(cb.construct(ConsolidationLiteResponse.class,
        root.get("id"),
        root.get("guid"),
        root.get("createdBy"),
        root.get("consolidationNumber"),
        root.get("consolidationType"),
        root.get("transportMode"),
        root.get("shipmentType"),
        root.get("isDomestic"),
        root.get("payment"),
        root.get("bookingCutoff"),
        root.get("estimatedTerminalCutoff"),
        root.get("terminalCutoff"),
        root.get("shipInstructionCutoff"),
        root.get("hazardousBookingCutoff"),
        root.get("verifiedGrossMassCutoff"),
        root.get("reeferCutoff"),
        root.get("referenceNumber"),
        root.get("bookingStatus"),
        root.get("bookingNumber"),
        root.get("mawb"),
        carrier.get("eta"),
        carrier.get("ata"),
        carrier.get("etd"),
        carrier.get("atd"),
        carrier.get("voyage"),
        carrier.get("shippingLine"),
        carrier.get("id"),
        carrier.get("vessel"),
        carrier.get("origin"),
        carrier.get("destination"),
        carrier.get("originPort"),
        carrier.get("destinationPort"),
        carrier.get("originLocCode"),
        carrier.get("destinationLocCode"),
        carrier.get("originPortLocCode"),
        carrier.get("destinationPortLocCode")
    ));

    // Execute query with pagination
    TypedQuery<ConsolidationLiteResponse> query = entityManager.createQuery(cq);
    query.setFirstResult((int) pageable.getOffset());
    query.setMaxResults(pageable.getPageSize());

    // Count total records for pagination
    CriteriaQuery<Long> countQuery = cb.createQuery(Long.class);
    Root<ConsolidationDetails> countRoot = countQuery.from(ConsolidationDetails.class);
    countQuery.select(cb.count(countRoot));
    if (spec != null) {
      Predicate countPredicate = spec.toPredicate(countRoot, countQuery, cb);
      if (countPredicate != null) {
        countQuery.where(countPredicate);
      } else {
        countQuery.where(cb.conjunction());
      }
    } else {
      countQuery.where(cb.conjunction());
    }
    long total = entityManager.createQuery(countQuery).getSingleResult();

    // Return paginated result
    List<ConsolidationLiteResponse> result = query.getResultList();
    return new PageImpl<>(result, pageable, total);
  }

}

