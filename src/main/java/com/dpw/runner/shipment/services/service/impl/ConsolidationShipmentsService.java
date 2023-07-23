//package com.dpw.runner.shipment.services.service.impl;
//
//import org.springframework.stereotype.Service;
//
//public class ConsolidationShipmentsService {
//
//    @Service
//    public class ParentChildService {
//        @Autowired
//        private ParentEntityRepository parentEntityRepository;
//
//        @Autowired
//        private ChildEntityRepository childEntityRepository;
//
//        public void attachChildrenToParent(Long parentId, List<Long> childIds) {
//            ParentEntity parent = parentEntityRepository.findById(parentId).orElseThrow(EntityNotFoundException::new);
//
//            for (Long childId : childIds) {
//                ChildEntity child = childEntityRepository.findById(childId).orElseThrow(EntityNotFoundException::new);
//                parent.getChildren().add(child);
//                child.getParents().add(parent);
//            }
//
//            parentEntityRepository.save(parent);
//        }
//
//        public void detachChildrenFromParent(Long parentId, List<Long> childIds) {
//            ParentEntity parent = parentEntityRepository.findById(parentId).orElseThrow(EntityNotFoundException::new);
//
//            for (Long childId : childIds) {
//                ChildEntity child = childEntityRepository.findById(childId).orElseThrow(EntityNotFoundException::new);
//                parent.getChildren().remove(child);
//                child.getParents().remove(parent);
//            }
//
//            parentEntityRepository.save(parent);
//        }
//    }
//}
