package serializer

import parser.Constant
import vm.ReductionTreeUnoptimized
import vm.ReductionTreeUnoptimized.*
import vm.Ownership.*

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import scala.collection.mutable

enum DeserializationError:
  case IdNotFound(id: Int)
  case UnknownConstTag(tag: Int)
  case UnknownNodeTag(tag: Int)
  case ExpectedApplication(got: ReductionTreeUnoptimized)

object SerializerUnoptimized {
  def serialize(tree: ReductionTreeUnoptimized): Array[Byte] =
    val baos = new ByteArrayOutputStream()
    val dos = new DataOutputStream(baos)

    val visitedNodes: mutable.Map[Int, Int] = mutable.Map()
    val nodesToSerialize: mutable.Map[Int, ByteArrayOutputStream] = mutable.Map()
    var nextId: Int = 0

    def writeNodeData(node: ReductionTreeUnoptimized): Int =
      visitedNodes.get(System.identityHashCode(node)) match
        case Some(id) =>
          // Node already visited, just return its ID (it will be referenced)
          id
        case None =>
          val currentId = nextId
          nextId += 1
          visitedNodes.put(System.identityHashCode(node), currentId)

          val nodeBaos = new ByteArrayOutputStream()
          val nodeDos = new DataOutputStream(nodeBaos)

          nodeDos.writeByte(node.ordinal)

          node match
            case Const(c) =>
              nodeDos.writeByte(c.ordinal)
              c match
                case Constant.Num(n) =>
                  nodeDos.writeLong(n)
                case Constant.Bool(b) =>
                  nodeDos.writeBoolean(b)
                case Constant.Str(s) =>
                  nodeDos.writeUTF(s)
                case Constant.Nil =>
            case op@(S | K | I | Y | U | Plus | Minus | Mul | Div | Leq | Lt | Geq | Gt | Eq | Neq | And | Or | Cond | Cons | Hd | Tl) =>
            case ReductionTreeUnoptimized.Application(operator, operand) =>
              nodeDos.writeByte(operator.ordinal)
              nodeDos.writeInt(writeNodeData(operator.inner))

              nodeDos.writeByte(operand.ordinal)
              nodeDos.writeInt(writeNodeData(operand.inner))
            case ReductionTreeUnoptimized.Pair(hd, tl) =>
              // we should never need to serialize pair
              assert(false)
            case ReductionTreeUnoptimized.Unresolved(name) =>
              // we should never need to serialize unresolved
              assert(false)

          nodeDos.flush()
          nodesToSerialize.put(currentId, nodeBaos)
          currentId

    val rootId = writeNodeData(tree)
    dos.writeInt(rootId)
    dos.writeInt(nodesToSerialize.size)

    nodesToSerialize.foreach { case (id, nodeBaos) =>
      dos.writeInt(id)
      val nodeBytes = nodeBaos.toByteArray
      dos.writeInt(nodeBytes.length)
      dos.write(nodeBytes)
    }

    dos.flush()
    baos.toByteArray

  def deserialize(bytes: Array[Byte]): Either[DeserializationError, ReductionTreeUnoptimized] =
    val bais = new ByteArrayInputStream(bytes)
    val dis = new DataInputStream(bais)

    val rootId = dis.readInt()
    val numberOfNodes = dis.readInt()

    val serializedNodeData: mutable.Map[Int, Array[Byte]] = mutable.Map()
    for _ <- 0 until numberOfNodes do
      val id = dis.readInt()
      val length = dis.readInt()
      val nodeBytes = new Array[Byte](length)
      dis.readFully(nodeBytes)
      serializedNodeData.put(id, nodeBytes)

    val createdInstances: mutable.Map[Int, ReductionTreeUnoptimized] = mutable.Map()
    val unresolvedApplications: mutable.Buffer[(Int, Int, Int, Int, Int)] = mutable.Buffer()

    for (id, nodeBytes) <- serializedNodeData do
      val nodeBais = new ByteArrayInputStream(nodeBytes)
      val nodeDis = new DataInputStream(nodeBais)
      val tag = nodeDis.readByte()

      if (S.ordinal <= tag && tag <= Tl.ordinal) {
        createdInstances.put(id, ReductionTreeUnoptimized.fromOrdinal(tag))
      } else if (tag == Const(Constant.Num(0)).ordinal) {
        val constTypeTag = nodeDis.readByte()
        val constant = constTypeTag match {
          case t if t == Constant.Num(0).ordinal => Constant.Num(nodeDis.readLong())
          case t if t == Constant.Bool(true).ordinal => Constant.Bool(nodeDis.readBoolean())
          case t if t == Constant.Nil.ordinal => Constant.Nil
          case t if t == Constant.Str("").ordinal => Constant.Str(nodeDis.readUTF())
          case _ => return Left(DeserializationError.UnknownConstTag(constTypeTag))
        }
        createdInstances.put(id, Const(constant))
      } else if (tag == Application(Strong(I), Strong(I)).ordinal) {
        val opOwnershipTag = nodeDis.readByte()
        val opId = nodeDis.readInt()
        val operandOwnershipTag = nodeDis.readByte()
        val operandId = nodeDis.readInt()
        // Store the IDs to resolve later
        createdInstances.put(id, Application(Strong(I), Strong(I)))
        unresolvedApplications.append((id, opOwnershipTag.toInt, opId, operandOwnershipTag.toInt, operandId))
      } else {
        return Left(DeserializationError.UnknownNodeTag(tag))
      }

    for ((appId, opOwnershipTag, opId, operandOwnershipTag, operandId) <- unresolvedApplications) {
      val operator = createdInstances.get(opId) match {
        case Some(rt) => rt
        case None => return Left(DeserializationError.IdNotFound(opId))
      }
      val operand = createdInstances.get(operandId) match {
        case Some(rt) => rt
        case None => return Left(DeserializationError.IdNotFound(operandId))
      }

      val ownedOperator = if opOwnershipTag == Strong(I).ordinal then Strong(operator) else Weak(operator)
      val ownedOperand = if operandOwnershipTag == Strong(I).ordinal then Strong(operand) else Weak(operand)
      createdInstances.get(appId) match {
        case Some(rt) =>
          rt match {
            case a @ Application(_, _) =>
              a.operator = ownedOperator
              a.operand = ownedOperand
            case _ =>
              return Left(DeserializationError.ExpectedApplication(rt))
          }
        case None => return Left(DeserializationError.IdNotFound(appId))
      }
    }

    createdInstances.get(rootId) match {
      case Some(node) => Right(node)
      case None => Left(DeserializationError.IdNotFound(rootId))
    }
}
